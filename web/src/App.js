import React from 'react';
import axios from 'axios';
import settings from './settings.js';
import Activity from './Activity.js';
var Utils = require('./Utils.js');

class App extends React.Component {
    constructor(props) {
	super(props);

	this.state = {
	    activities: {},
	    user: undefined,
	    editedActivity: null,
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

    getActivities() {
	this.state.api.get('/activities')
	    .then((response) =>
		  this.setState({ activities: Utils.mergeActivities(response.data[0], response.data[1]) }))
	    .catch((error) => console.log("getActivities: request failed with error ", error));
    }

    newActivity() {
	return {
	    "title": "Rubrik (ändra mig)",
	    "description": "Beskrivning (ändra mig)",
	    "date": "(ändra mig)",
	    "host":"",
	    "attendees": [],
	    "meetingPoint": "(ändra mig)",
	    "editPassword": ""
	};
    }

    addActivity() {
	let newActivity = this.newActivity();
	newActivity.host = this.state.user;
	const activities = Object.assign(this.state.activities, null);
	activities.new = newActivity;
	this.setState({activities: activities, editedActivity: "new"});
    }

    addAttendee() {
	alert("Not implemented yet");
    }

    /* TODO: Change post to send application/x-www-form-urlencoded instead of JSON */
    saveActivity(actId, activity) {
	if (actId !== 'new') {
	    this.state.api.put('/activity/' + actId, Utils.urlencode(activity))
		.then(this.getActivities())
		.catch((error) => console.log('Update error: ', error));
	} else {
	    this.state.api.post('/activity/', Utils.urlencode(activity), )
		.then(this.getActivities())
		.catch((error) => console.log('Create error:', error));
	}
    }

    removeActivity(actId) {
	if (actId !== 'new') {
	    this.state.api.delete('/activity/' + actId)
		.then(this.getActivities())
		.catch((error) => console.log(error));
	} else {
	    let activities = Object.assign(this.state.activities, null);
	    delete activities.new;
	    this.setState({activities: activities});
	}
    }

    render() {
	let content = this.state.user ? this.renderActivities() : this.renderLogin();

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
	return (
	    <footer>
	      <p>Skapad av Den Gode Pastorn</p>
	    </footer>
	);
    }

    /* TODO: Add localStorage with session cookie */
    renderLogin() {
	return (
	    <LoginForm
	      callback={(response) => {
		  this.getActivities();
		  this.setState({user: response});
	      }}
	      api={this.state.api} />
	);
    }

    renderActivities() {
	const activities = [];
	for (let actId in this.state.activities) {
	    const editable = this.state.editedActivity === actId ? true : false;
	    activities.push(
		<Activity
		  activity={this.state.activities[actId]}
		  addAttendee={() => this.addAttendee(actId)}
		  saveActivity={() => this.saveActivity(actId)}
		  removeActivity={() => this.removeActivity(actId)}
		  editable={editable}
		  new={editable}
		  key={actId} />
	    );
	}
	return (
	    <div>
	      {activities}
	      <button onClick={() => this.addActivity()}>
		Lägg till ny aktivitet
	      </button>
	    </div>
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
