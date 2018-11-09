import React from 'react';
import axios from 'axios';
import settings from './settings.js';
import Activity from './Activity.js';
var Utils = require('./Utils.js');

class App extends React.Component {
    constructor(props) {
	super(props);

	this.getActivities();

	this.state = {
	    activities: {},
	    editedActivity: null,
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

export default App;
