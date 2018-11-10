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
	};
    }

    componentWillMount(){
	this.getActivities();
    }

    getActivities() {
	axios.get(settings.baseURL + '/activities')
	    .then((response) =>
		  this.setState({ activities: Utils.mergeActivities(response.data[0], response.data[1]) }))
	    .catch((error) => console.log("axios failed: ", error));
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
	    axios.put(settings.baseURL + '/activity/' + actId, activity)
		.then(this.getActivities())
		.catch((error) => console.log('Update error: ', error));
	} else {
	    axios.post(settings.baseURL + '/activity/', activity)
		.then(this.getActivities())
		.catch((error) => console.log('Create error:', error));
	}
    }

    removeActivity(actId) {
	if (actId !== 'new') {
	    axios.delete(settings.baseURL + '/activity/' + actId)
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
	    <LoginForm callback={(response) => this.setState({user: response})} />
	);
    }

    renderActivities() {
	const activities = [];
	for (let actId in this.state.activities) {
	    const editable = this.state.editedActivity === actId ? true : false;
	    activities.push(<Activity
			    activity={this.state.activities[actId]}
			    addAttendee={() => this.addAttendee(actId)}
			    saveActivity={() => this.saveActivity(actId)}
			    removeActivity={() => this.removeActivity(actId)}
			    editable={editable}
			    new={editable}
			    key={actId} />);
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
	    email: "",
	    password: "",
	    errors: [],
	};
    }

    render() {
	let errItems = this.state.errors.map((err, k) => <li>{err}</li>);
	return (
	    <div>
	      <ul>{errItems}</ul>
	      <label>E-postadress:</label>
	      <input name="email"
		     type="text"
		     onChange={(event) => this.setState({email: event.target.value})}>
	      </input>
	      <label>Lösenord:</label>
	      <input name="password" type="password"
		     onChange={(event) => this.setState({password: event.target.value})}>
	      </input>
		<button onClick={() => this.checkLogin()}>Logga in</button>
	    </div>
	);
    }

    checkLogin() {
	let payload = encodeURIComponent('loginEmail=' + this.state.email + '&loginPassword=' + this.state.password);
	axios.post(settings.baseURL+'/login',
	 	   payload, { headers: {'Content-Type': 'application/x-www-form-urlencoded'},
	 		      responseType: 'application/json' })
	    .then((response) => {console.log(response);
	 			 this.props.callback(response);})
	    .catch((error) => console.log("Failed to login", error));
    }
}

export default App;
