import React, { Component } from 'react';

import Activity from './Activity.js';
import Utils from './Utils.js';

class ActivityList extends Component {
    constructor(props) {
	super(props);

	this.state = {
	    activities: {},
	    editedActivity: null,
	};
    }

    componentWillMount() {
	this.reloadActivities();
    }

    getActivities(callback, errorHandler) {
	this.props.api.get('/activities')
	    .then((response) => callback(Utils.mergeActivities(response.data[0], response.data[1])))
	    .catch((error) => errorHandler(error));
    }

    reloadActivities() {
        this.getActivities((activities) => this.setState({activities: activities}),
                           (errors) => console.log(errors));
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
	newActivity.host = this.props.user;
	const activities = Object.assign(this.state.activities, null);
	activities.new = newActivity;
	this.setState({activities: activities, editedActivity: "new"});
    }

    addAttendee(actId) {
	let activities = Object.assign(this.state.activities, null);
	if (activities[actId].attendees[this.props.user.email]) {
	    console.log("Already attends");
	} else {
	    let activity = Utils.unmergeActivity(activities[actId]);
            activity.attendees.push(this.props.user.email);
            this.props.api.post('/activity/' + actId, Utils.urlencode(activity))
                .then(this.reloadActivities());
	}
    }

    /* TODO: Change post to send application/x-www-form-urlencoded instead of JSON */
    saveActivity(actId, activity) {
	if (actId !== 'new') {
	    this.props.api.post('/activity/' + actId, Utils.urlencode(activity))
		.then(this.getActivities())
		.catch((error) => console.log('Update error: ', error));
	} else {
	    this.props.api.post('/activity/', Utils.urlencode(activity), )
		.then(this.getActivities())
		.catch((error) => console.log('Create error:', error));
	}
    }

    removeActivity(actId) {
	if (actId !== 'new') {
	    this.props.api.delete('/activity/' + actId)
		.then(this.getActivities())
		.catch((error) => console.log(error));
	} else {
	    let activities = Object.assign(this.state.activities, null);
	    delete activities.new;
	    this.setState({activities: activities});
	}
    }

    render() {
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
	      <button onClick={() => this.props.addActivity()}>
		Lägg till ny aktivitet
	      </button>
	    </div>
	);
    }
}

export default ActivityList;
