import React, { Component } from 'react';
import Grid from '@material-ui/core/Grid';
import Button from '@material-ui/core/Button';

import Activity from './Activity.js';
import Utils from './Utils.js';

class ActivityList extends Component {
    constructor(props) {
	super(props);

	this.state = {
	    activities: {},
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
        this.getActivities((activities) => this.setState({activities: activities}), error => console.log(error));
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

    toggleAttendence(actId) {
        let activity = Object.assign(this.state.activities[actId], null);
	if (activity.attendees[this.props.user.email]) {
            delete activity.attendees[this.props.user.email];
	} else {
            activity.attendees[this.props.user.email] = this.props.user;
	}
        this.saveActivity(actId, activity);
    }

    saveActivity(actId, activity) {
	let postURL = actId === 'new' ? '/activities' : '/activity/' + actId;
        // Extremely hackish solution, forces the rendering of the ActivityList to be redone
        let activities = this.state.activities;
        delete activities[actId];
        this.setState({activities: activities});
	this.props.api.post(postURL, Utils.urlencode(Utils.unmergeActivity(activity)))
	    .then(this.reloadActivities());
    }

    removeActivity(actId) {
	if (actId !== 'new') {
	    this.props.api.delete('/activity/' + actId)
		.then(this.reloadActivities());
	} else {
	    let activities = Object.assign(this.state.activities, null);
	    delete activities.new;
	    this.setState({activities: activities});
	}
    }

    render() {
	const activities = [];
	for (let actId in this.state.activities) {
	    activities.push(
		<Grid item xs={12}><Activity
		  activity={this.state.activities[actId]}
                  user={this.props.user}
		  toggleAttendence={() => this.toggleAttendence(actId)}
		  saveActivity={(activity) => this.saveActivity(actId, activity)}
		  removeActivity={() => this.removeActivity(actId)}
		  isOwner={this.state.activities[actId].host.email === this.props.user.email}
		  isNew={actId === 'new'}
		  key={actId} /></Grid>
	    );
	}
	return (
            <Grid container spacing={16} justify="center">
	      {activities}
	      <Grid item>
                <Button size="small" variant="contained" color="primary" onClick={() => this.addActivity()}>
	          Lägg till aktivitet
	        </Button>
              </Grid>
            </Grid>
	);
    }
}

export default ActivityList;
