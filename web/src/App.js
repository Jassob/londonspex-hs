import React from 'react';
import axios from 'axios';
import settings from './settings.js';
var Utils = require('./Utils.js');

class App extends React.Component {
import ActivityList from './ActivityList.js';
import LoginForm from './LoginForm.js';
    constructor(props) {
	super(props);

	this.state = {
	    user: JSON.parse(Utils.storageRead("user")),
	    api: this.createApi(),
	};
    }

    createApi() {
	const api = axios.create({
	    'baseURL': settings.baseURL,
	});

	api.defaults.headers.post['Content-Type'] = 'application/x-www-form-urlencoded';
	api.defaults.headers.put['Content-Type'] = 'application/x-www-form-urlencoded';
	return api;
    }

    logout() {
	Utils.storageDelete("user");
	this.setState({"user": undefined});
    }

    render() {
	let content = this.state.user ? this.renderActivities() : this.renderLogin();
	let logoutButton = this.state.user ? <button onClick={() => this.logout()}>Logga ut</button> : "";
	return (
	    <div>
	      {this.renderHeader()}
	      {content}
	      {this.renderFooter()}
	    </div>
	);
    }

    renderHeader() {
	return (
	    <header>
		<h1>Aktiviteter - London</h1>
		<h2>Chalmersspexet Bob 2018 Geronimo</h2>
	      </header>
	);
    }

    renderFooter() {
	let logoutButton = "";
	if (this.state.user) {
	    logoutButton = (
		<button onClick={() => {
		    Utils.storageDelete("user");
		    this.setState({"user": undefined});
		}}>Logga ut</button>
	    );
	}
	return (
	    <footer>
	      <p>Skapad av Den Gode Pastorn</p>
	      {logoutButton}
	    </footer>
	);
    }

    /* TODO: Add localStorage with session cookie */
    renderLogin() {
	return (
	    <LoginForm
	      callback={(response) => {
		  Utils.storageStore("user", JSON.stringify(response));
		  this.setState({user: response});
	      }}
	      api={this.state.api} />
	);
    }

    renderActivities() {
	return (
	    <ActivityList
	      user={this.state.user}
	      api={this.state.api}
	      />
	);
    }
}

export default App;
