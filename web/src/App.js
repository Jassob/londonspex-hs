import React from 'react';
import axios from 'axios';
import settings from './settings.js';
var Utils = require('./Utils.js');

class App extends React.Component {
    constructor(props) {
	super(props);

	this.getActivities();

	this.state = {
	    activities: {},
	};
    }

    getActivities() {
	axios.get(settings.baseURL + '/activities')
	    .then((response) =>
		  this.setState({ activities: this.mergeActivities(response) }))
	    .catch((error) => console.log("axios failed: ", error));
    }

    mergeActivities(response) {
	let persons = response.data[1];
	let replaceAttendee = (attendees, attendeeId) => {
	    attendees[attendeeId] = persons[attendeeId];
	    return attendees;
	};
	return Utils.objectMap(response.data[0], (act) => {
	    act.host = persons[act.host];
	    act.attendees = act.attendees.reduce(replaceAttendee, {});
	    return act;
	});
    }

    addActivity() {
	alert("Not implemented yet");
    }

    addAttendee() {
	alert("Not implemented yet");
    }

    saveActivity() {
	alert("Not implemented yet");
    }

    removeActivity() {
	alert("Not implemented yet");
    }

    render() {
	let activities = [];
	for (let actId in this.state.activities) {
	    activities.push(<Activity
			    activity={this.state.activities[actId]}
			    addAttendee={() => this.addAttendee(actId)}
			    saveActivity={() => this.saveActivity(actId)}
			    removeActivity={() => this.removeActivity(actId)}
			    key={actId} />);
	}
	return (
	    <div>
	      <header>
		<h1>Aktiviteter - London</h1>
		<h2>Chalmersspexet Bob 2018 Geronimo</h2>
	      </header>
	      {activities}
	      <button onClick={() => this.addActivity()}>
		Lägg till ny aktivitet
	      </button>
	      <footer>
		<p>Skapad av Den Gode Pastorn</p>
	      </footer>
	    </div>
	);
    }
}

/**
 * Activity class renders an either expanded or minimized activity.
 *
 * Properties:
 * - 'activity': An activity returned from the backend
 * - 'addAttendee': A function for showing interest in this activity
 * - 'removeAttendee': A function for removing an attendee
 * - 'saveActivity': A function for saving an edited activity
 */
class Activity extends React.Component {
    constructor(props) {
	super(props);

	this.state = {
	    isExpanded: false,
	    edited: false,
	    editedActivity: props.activity,
	};
    }

    toggleExpand() {
	this.setState({
	    isExpanded: !this.state.isExpanded,
	});
    }

    renderMinimal() {
	return (
	    <div>
	      <header>
		<h3>{this.props.activity.date} - {this.props.activity.title}</h3>
		<p>{this.props.activity.description}</p>
		<button onClick={() => this.toggleExpand()}>Öppna aktivitet</button>
	      </header>
	      <button onClick={() => this.props.addAttendee()}>
		Anmäl intresse
	      </button>
	    </div>
	);
    }

    renderFull() {
	let mapAttendees = (obj, func) => Utils.objectToList(Utils.objectMapWithKey(obj, func));
	let attendeeDisp = mapAttendees(this.props.activity.attendees,
					(k, v) => <li><Attendee person={v} key={k} onClick={() => this.props.removeAttendee(k)} /></li>);
	console.log(attendeeDisp);
	const editable = this.state.edited;
	const activity = this.state.edited ? this.state.editedActivity : this.props.activity;
	return (
	    <div>
	      <header>
		<h3 contentEditable={editable}>{activity.title}</h3>
		<p contentEditable={editable}>{activity.description}</p>
	      </header>
	      <dl>
		<dt>Ansvarig</dt>
		<dd contentEditable={editable}>{activity.host.name}</dd>
		<dt>Starttid</dt>
		<dd contentEditable={editable}>{activity.date}</dd>
		<dt>Mötesplats</dt>
		<dd contentEditable={editable}>{activity.meetingPlace}</dd>
		<dt>Deltagare</dt>
		<dd>
		  <ul>{attendeeDisp}</ul>
		</dd>
	      </dl>
	      <button onClick={() => this.mailAttendees()}>
		Maila alla deltagare
	      </button>
	      <button onClick={() => this.toggleExpand()}>Stäng aktivitet</button>
	      <button onClick={() => this.setState({edited: true})}>Ändra aktivitet</button>
	      <button onClick={() => this.props.saveActivity(this.state.edited)}>Spara aktivitet</button>
	      <button onClick={() => this.props.removeActivity()}>Ta bort aktivitet</button>
	    </div>
	);
    }

    render() {
	if (this.state.isExpanded) {
	    return this.renderFull();
	} else {
	    return this.renderMinimal();
	}
    }
}

function Attendee(props) {
    return <p onClick={props.onClick}>{props.person.name}</p>;
}

export default App;
