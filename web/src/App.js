import React, { Component } from 'react';
import axios from 'axios';
import settings from './settings.js';

import ActivityList from './ActivityList.js';
import LoginForm from './LoginForm.js';
import Utils from './Utils.js';

class App extends Component {
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
	      <header>
		<h1>Aktiviteter - London</h1>
		<h2>Chalmersspexet Bob 2018 Geronimo</h2>
	      </header>
	      {content}
	      <footer>
		<p>Skapad av <a href="mailto://jacob.t.jonsson@gmail.com">Den Gode Pastorn</a></p>
		{logoutButton}
	      </footer>
	    </div>
	);
    }

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
