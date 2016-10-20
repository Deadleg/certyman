import * as React from "react";
import * as ReactDOM from "react-dom";
import { Router, Route, Link, browserHistory } from "react-router";

declare var getCerts;
declare var deleteCertsByDomain;

var Home = React.createClass({
    getInitialState: () => {
        return {
            certs: []
        }
    },

    componentDidMount() {
        var self = this;
        getCerts((data) => {
            console.log(data);
            self.setState({certs: data});
        });
    },

    deleteCert: function(domain: string) {
        var self = this;

        deleteCertsByDomain(domain, () => {
            alert("deleted");
            getCerts((data) => {
                console.log(data);
                self.setState({certs: data});
            });
        });
    },

    render: function() {
        var self = this;

        var certs = this.state.certs.map((cert, index) => {
            var container : any = JSON.parse(cert.container);
            var networks = container["com.github.lemanager.inspect"].NetworkSettings.Networks

            var ip;
            for (var key in networks) {
                ip = networks[key].IPAddress;
            }

            return (
                <tr key={index}>
                    <td className="mdl-data-table__cell--non-numeric">{cert.domain}</td>
                    <td className="mdl-data-table__cell--non-numeric">{cert.expireDate}</td>
                    <td className="mdl-data-table__cell--non-numeric">{ip}</td>
                    <td className="mdl-data-table__cell--non-numeric">{container.id.substring(0, 16)}</td>
                    <td className="mdl-data-table__cell--non-numeric">
                        <button className="mdl-button mdl-js-button mdl-button--raised">Reset</button>
                    </td>
                    <td className="mdl-data-table__cell--non-numeric">
                        <button className="mdl-button mdl-js-button mdl-button--raised" onClick={self.deleteCert.bind(self, cert.domain)}>Delete</button>
                    </td>
                </tr>
            );
        });

        return (
           <div className="mdl-layout mdl-js-layout mdl-layout--fixed-header">
             <header className="mdl-layout__header">
               <div className="mdl-layout__header-row">
                 <span className="mdl-layout-title">Let's Encrypt certificate manager</span>
                 <div className="mdl-layout-spacer"></div>
                 <nav className="mdl-navigation mdl-layout--large-screen-only">
                   <a className="mdl-navigation__link" href="">Link</a>
                 </nav>
               </div>
             </header>
             <div className="mdl-layout__drawer">
               <span className="mdl-layout-title">Title</span>
               <nav className="mdl-navigation">
                 <a className="mdl-navigation__link" href="">Link</a>
               </nav>
             </div>
             <main className="mdl-layout__content">
               <div className="page-content">
                 <div className="container mdl-grid">
                    <div className="mdl-cell mdl-cell--12-col">
                        <table className="mdl-data-table mdl-js-table">
                            <thead>
                                <tr>
                                    <th className="mdl-data-table__cell--non-numeric">domain</th>
                                    <th className="mdl-data-table__cell--non-numeric">expire date</th>
                                    <th className="mdl-data-table__cell--non-numeric">ip</th>
                                    <th className="mdl-data-table__cell--non-numeric">container</th>
                                    <th className="mdl-data-table__cell--non-numeric"></th>
                                    <th className="mdl-data-table__cell--non-numeric"></th>
                                </tr>
                             </thead>
                             <tbody>
                                {certs}
                             </tbody>
                        </table>
                      </div>
                  </div>
               </div>
             </main>
           </div>
        );
    }
});

ReactDOM.render((
        <Router history={browserHistory}>
            <Route path='/*' component={Home}/>
        </Router>
    ),
    document.getElementById('container')
);