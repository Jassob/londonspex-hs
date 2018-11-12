import React from 'react';

var Utils = require('./Utils.js');

/**
 * Activity class renders an either expanded or minimized activity.
 *
 * Properties:
 * - 'activity': An activity returned from the backend
 * - 'toggleAttendence': A function for showing interest in this activity
 * - 'removeAttendee': A function for removing an attendee
 * - 'saveActivity': A function for saving an edited activity
 */
class Activity extends React.Component {
    constructor(props) {
	super(props);

	this.state = {
	    isExpanded: false,
	    editing: false,
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
	      </header>
              {this.buttonSet()}
	    </div>
	);
    }

    buttonSet() {
        if (this.state.editing) {
            return this.renderEditingButtons();
        } else if (this.state.isExpanded) {
            return this.renderExpandedButtons();
        } else {
            return this.renderMinimalButtons();
        }
    }

    renderFull(activity) {
	let mapAttendees = (obj, func) => Utils.objectToList(Utils.objectMapWithKey(obj, func));
	let constructAttendee = (k, v) => <li><Attendee person={v} key={k}/></li>;
	let attendeeDisp = mapAttendees(this.props.activity.attendees, constructAttendee);
	return (
	    <div>
	      <header>
		<h3 contentEditable={this.state.editing}>{activity.title}</h3>
		<p contentEditable={this.state.editing}>{activity.description}</p>
	      </header>
	      <dl>
		<dt>Ansvarig</dt>
		<dd contentEditable={this.state.editing}>{activity.host.name}</dd>
		<dt>Starttid</dt>
		<dd contentEditable={this.state.editing}>{activity.date}</dd>
		<dt>Mötesplats</dt>
		<dd contentEditable={this.state.editing}>{activity.meetingPoint}</dd>
		<dt>Deltagare</dt>
		<dd>
		  <ul>{attendeeDisp}</ul>
		</dd>
	      </dl>
	      {this.mailLink()}
	      {this.buttonSet()}
	    </div>
	);
    }

    render() {
	if (this.state.isExpanded || this.state.editing) {
            let activity = this.state.edithing ? this.state.editedActivity : this.props.activity;
	    return this.renderFull(activity);
	} else {
	    return this.renderMinimal();
	}
    }

    mailLink() {
        if (this.props.isOwner) {
	    const flattenAttendees = (attendees, func) => Utils.objectToList(Utils.objectMap(attendees, func)).join(",");
	    const mailAddresses = flattenAttendees(this.props.activity.attendees, (attendee) => attendee.email);
	    return <a href={'"mailto://' + mailAddresses + '"'}>Maila alla deltagare</a>;
        } else {
            return <a href={'"mailto://' + this.props.activity.host.email + '"'}>Maila ansvarig</a>;
        }
    }

    /* Buttons */
    renderEditingButtons() {
        return (
            <>
              <Button onClick={() => this.props.saveActivity(this.state.editedActivity)}
                      text={"Spara aktivitet"} />
              {this.toggleEditingButton()}
              {this.removeButton()}
            </>
        );
    }

    renderExpandedButtons() {
        if (this.props.isOwner) {
            return (
                <>
                  {this.toggleEditingButton()}
                  {this.toggleExpandButton()}
                  {this.removeButton()}
                </>
            );
        } else {
            return (
                <>
                  {this.attendenceButton()}
                  {this.toggleExpandButton()}
                </>
            );
        }
    }

    renderMinimalButtons() {
        return (
            <>
              {this.toggleExpandButton()}
              {this.attendenceButton()}
            </>
        );
    }

    attendenceButton() {
        return (
            <Button onClick={() => this.props.toggleAttendence()}
                    text={this.props.activity.attendees[this.props.user.email]
                          ? "Avanmäl intresse" : "Anmäl intresse"} />
        );
    }

    removeButton() {
        return (
            <Button onClick={() => this.props.removeActivity()}
                    text={"Ta bort aktivitet"}/>
        );
    }

    toggleExpandButton() {
        return (
            <Button onClick={() => this.toggleExpand()}
              text={this.state.isExpanded ? 'Stäng aktivitet' : 'Öppna aktivitet'} />
        );
    }

    toggleEditingButton() {
        return (
            <Button onClick={() => this.setState({editing: !this.state.editing})}
                    text={this.state.editing ? 'Avbryt' : 'Ändra aktivitet'} />
        );
    }

}

function Attendee(props) {
    return <p onClick={props.onClick}>{props.person.name}</p>;
}

function Button(props) {
    return <button onClick={props.onClick}>{props.text}</button>;
}

export default Activity;
