import React from 'react';

var Utils = require('./Utils.js');

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
	    isEditable: this.props.editable,
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
		<button onClick={() => this.toggleExpand()}>
		  Öppna aktivitet
		</button>
	      </header>
	      <button onClick={() => this.props.addAttendee()}>
		Anmäl intresse
	      </button>
	    </div>
	);
    }

    renderFull() {
	let mapAttendees = (obj, func) => Utils.objectToList(Utils.objectMapWithKey(obj, func));
	let constructAttendee = (k, v) =>
	    <li><Attendee person={v} key={k} onClick={() => this.props.removeAttendee(k)} /></li>;
	let attendeeDisp = mapAttendees(this.props.activity.attendees, constructAttendee);
	const editable = this.state.isEditable;
	const activity = this.state.isEditable ? this.state.editedActivity : this.props.activity;
	let editButtons = (
	    <div>
	      <button onClick={() => this.props.saveActivity(this.state.isEditable)}>
		Spara aktivitet
	      </button>
	      <button onClick={() => this.props.new ? this.props.removeActivity('new') : this.setState({isEditable: false})}>
		Avbryt
	      </button>
	    </div>
	);
	let normalButtons = (
	    <div>
	      <button onClick={() => this.toggleExpand()}>Stäng aktivitet</button>
	      <button onClick={() => this.setState({isEditable: true})}>Ändra aktivitet</button>
	      <button onClick={() => this.props.removeActivity()}>Ta bort aktivitet</button>
	    </div>
	);
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
		<dd contentEditable={editable}>{activity.meetingPoint}</dd>
		<dt>Deltagare</dt>
		<dd>
		  <ul>{attendeeDisp}</ul>
		</dd>
	      </dl>
	      <a href={this.mailAttendees()}>
		Maila alla deltagare
	      </a>
	      {this.state.isEditable ? editButtons : normalButtons}
	    </div>
	);
    }

    render() {
	if (this.state.isExpanded || this.state.isEditable) {
	    return this.renderFull();
	} else {
	    return this.renderMinimal();
	}
    }

    mailAttendees() {
	const flattenAttendees = (attendees, func) => Utils.objectToList(Utils.objectMap(attendees, func)).join(",");
	const mailAddresses = flattenAttendees(this.props.activity.attendees, (attendee) => attendee.email);
	return "mailto://" + mailAddresses;
    }
}

function Attendee(props) {
    return <p onClick={props.onClick}>{props.person.name}</p>;
}

export default Activity;
