import React, { Component } from 'react';

import Utils from './Utils.js';

class LoginForm extends Component {
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
	    .catch((error) => console.log(this.state.errors, error));
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

export default LoginForm;
