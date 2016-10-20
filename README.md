# Certyman

A convoluted way to get certificates with Docker via Let's Encrypt.

## Purpose
 
This is yet a another free certificate manager based around Docker. The main thing this does differently is that the certificates are stored in a file, but rather a Redis instance. This allows you to run multiple instances of Nginx/equivalent and serve the same certificate without requesting new ones on the first request.

Certyman runs on just Docker with the help of labeling, with the downside that it's not the most friendly thing to work with since you don't have a framework behind you. There are no releases for Certyman since this was more of an experiment, so if you want to try it out you'll need to build it yourself.

## How it works

1. Start a docker web server with `certyman.domain` set to the domain oyu want
2. [docker-event-emitter](https://github.com/Deadleg/docker-event-emitter) with pick up the event, and send it to Redis
3. `Certyman-listener` to receive the event and grab the `certyman.domain` domain along with other networking information Docker provides and put that data into Redis.
4. (In my case) Openresty with search Redis for the list of domains to be served, and generate certificates if there are none. Certificates are stored in redis as well.

## Usage

- You'll need to run [docker-event-emitter](https://github.com/Deadleg/docker-event-emitter) on each of your docker hosts.
- Have a redis instance running.
- Run one `certyman-listener` with access to Redis
- (Optional) Run `certyman-ui` for a web page to view the list of domains served.

Once the above are running, all you will need to do is start your Docker containers with the `certyman.domain` set to the domain you wish to route to with TLS (note you cannot use wildcards from Let's Encrypt).

Using Openresty, you can run something like the following to get your certificates served (see [lua-resty-auto-ssl](https://github.com/GUI/lua-resty-auto-ssl)):

```nginx
events {
    worker_connections 1024;
}

http {
    # The "auto_ssl" shared dict must be defined with enough storage space to
    # hold your certificate data.
    lua_shared_dict auto_ssl 1m;

    # A DNS resolver must be defined for OSCP stapling to function.
    resolver 8.8.8.8;

    # Initial setup tasks.
    init_by_lua_block {
        auto_ssl = (require "resty.auto-ssl").new()

        -- Define a function to determine which SNI domains to automatically handle
        -- and register new certificates for. Defaults to not allowing any domains,
        -- so this must be configured.
        auto_ssl:set("allow_domain", function(domain)
            local resty_redis = require "resty.redis"
            local redis = resty_redis:new()
            local log = ngx.log
            local ERR = ngx.ERR

            redis:set_timeout(10000)

            local ok, err = redis:connect("172.16.238.15", 6379)
            if not ok then
                log(ERR, "Error connecting to redis: " .. err)
                redis:set_keepalive(1000, 100)
                return false
            end

            local is_member, err = redis:sismember("lemanager:domains", domain)
            if err then
                log(ERR, "Error checkout if " .. domain .. " is in lemanager:domains: " .. err)
                redis:set_keepalive(1000, 100)
                return false
            end

            log(ERR, is_member ~= 0)
            redis:set_keepalive(10000, 100)
            return is_member ~= 0
        end)

        auto_ssl:set("storage_adapter", "resty.auto-ssl.storage_adapters.redis")

        auto_ssl:set("redis", {
            host = "172.16.238.15",
            port = 6379
        })

        auto_ssl:init()
    }

    init_worker_by_lua_block {
        auto_ssl:init_worker()
    }

    # admin
    server {
        listen 443 ssl;
        server_name <domain for certy-ui>;
        allow 58.28.155.208;

        ssl_certificate_by_lua_block {
            auto_ssl:ssl_certificate()
        }

        ssl_certificate /etc/ssl/resty-auto-ssl-fallback.crt;
        ssl_certificate_key /etc/ssl/resty-auto-ssl-fallback.key;

        location / {
            proxy_pass http://ui:8001;
        }
    }

    # HTTPS server
    server {
        listen 443 ssl;
        server_name _;

        ssl_certificate_by_lua_block {
            auto_ssl:ssl_certificate()
        }

        ssl_certificate /etc/ssl/resty-auto-ssl-fallback.crt;
        ssl_certificate_key /etc/ssl/resty-auto-ssl-fallback.key;

        location / {
            set $new_host '';

            access_by_lua_block {
                local resty_redis = require "resty.redis"
                local redis = resty_redis:new()
                local log = ngx.log
                local ERR = ngx.ERR

                redis:set_timeout(10000)

                local ok, err = redis:connect("172.16.238.15", 6379)
                if not ok then
                    log(ERR, "Error connecting to redis: " .. err)
                    redis:set_keepalive(10000, 100)
                    return
                end
                
                log(ERR, ngx.var.host)
                local new_host, err = redis:smembers("lemanager:domains:" .. ngx.var.host)
                if err then
                    log(ERR, "Error find upstream")
                    redis:set_keepalive(10000, 100)
                    return
                end

                redis:set_keepalive(10000, 100)
                ngx.var.new_host = "http://" .. new_host[1]
            }

            proxy_pass $new_host;
        }
    }

    # HTTP server
    server {
        listen 80;

        # Endpoint used for performing domain verification with Let's Encrypt.
        location /.well-known/acme-challenge/ {
            content_by_lua_block {
                auto_ssl:challenge_server()
            }
        }
    }

    # Internal server running on port 8999 for handling certificate tasks.
    server {
        listen 127.0.0.1:8999;
        location / {
            content_by_lua_block {
                auto_ssl:hook_server()
            }
        }
    }
}
```
