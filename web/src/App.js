import React from 'react';
import axios from 'axios';
import settings from './settings.js';
var Utils = require('./Utils.js');

class App extends React.Component {
import ActivityList from './ActivityList.js';
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
		  Utils.store("user", JSON.stringify(response));
		  this.getActivities();
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

class LoginForm extends React.Component {
    constructor(props) {
	super(props);

	this.state = {
	    errors: [],
	    showRegister: false,
	    name: "",
	    email: "",
	    password: "",
	    phone: "",
	};
    }

    handleFormUpdate(event) {
	let state = {}; state[event.target.name] = event.target.value;
	this.setState(state);
    }

    render() {
	let errItems = this.state.errors.map((err, k) => <li>{err}</li>);
	let content = this.state.showRegister ? this.renderRegister() : this.renderLogin();
	return (
	    <div>
	      <ul>{errItems}</ul>
	      {content}
	    </div>
	);
    }

    renderLogin() {
	return (
	    <div>
	      <h3>Logga in</h3>
	      <label>E-postadress:</label>
	      <input name="email"
		     type="text"
		     onChange={(event) => this.handleFormUpdate(event)}>
	      </input> <br />
	      <label>Lösenord:</label>
	      <input name="password" type="password"
		     onChange={(event) => this.handleFormUpdate(event)}>
	      </input> <br />
	      <button onClick={() => this.checkLogin()}>Logga in</button> <br />
	      <label>Har du inget konto?</label>
	      <button onClick={() => this.setState({showRegister: true})}>Registrera</button>
	    </div>
	);
    }

    renderRegister() {
	return (
	    <div>
	      <h3>Registrera dig</h3>
	      <label>Namn:</label>
	      <input name="name"
		     type="text"
		     onChange={(event) => this.handleFormUpdate(event)}>
	      </input> <br />
	      <label>Email:</label>
	      <input name="email"
		     type="email"
		     onChange={(event) => this.handleFormUpdate(event)}>
	      </input> <br />
	      <label>Lösenord:</label>
	      <input name="password"
		     type="password"
		     onChange={(event) => this.handleFormUpdate(event)}>
	      </input> <br />
	      <button onClick={() => this.checkRegister()}>Registrera</button>
	      <br />
	      <label>Redan registrerad?</label>
	      <button onClick={() => this.setState({showRegister: false})}>Logga in</button>
	    </div>
	);
    }

    checkLogin() {
	let payload = Utils.urlencode({ loginEmail: this.state.email, loginPassword: this.state.password });
	this.props.api.post('/login', payload)
	    .then((response) => this.props.callback(response.data))
	    .catch((error) => this.setState({errors: this.state.errors.slice().push("Okänt fel vid inloggning.")}));
    }

    checkRegister() {
	let errors = [];
	if (this.state.name === "") {
	    errors.push('Du måste ange ett namn.');
	}
	if (this.state.email === "") {
	    errors.push('Du måste ange en e-postadress.');
	}
	if (!this.state.email.includes("@")) {
	    errors.push("Ogiltig e-postadress, måste vara på formen 'användare@domän.tld'");
	}
	if (this.state.password === "") {
	    errors.push('Du måste ange ett lösenord');
	}
	if (errors.length > 0) {
	    this.setState({errors: errors});
	    return;
	}
	let payload = Utils.urlencode({ email: this.state.email,
					name: this.state.name,
					password: this.state.password });
	this.props.api.post('/register', payload)
	    .then((response) => this.props.callback(response.data))
	    .catch((errors) => this.setState({errors: this.state.errors.slice().push(errors)}));
    }
}

export default App;
