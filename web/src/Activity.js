import React from 'react';
import Grid from '@material-ui/core/Grid';
import Card from '@material-ui/core/Card';
import CardActions from '@material-ui/core/CardActions';
import CardContent from '@material-ui/core/CardContent';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';
import DeleteIcon from '@material-ui/icons/Delete';
import Icon from '@material-ui/core/Icon';
import TextField from '@material-ui/core/TextField';

import './root.css';
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
	    editing: false,
            activity: props.activity,
	};
    }

    renderCardHeader() {
        return (
            <>
              <Grid item xs={6} md={4}>
                <Grid container>
                  <Grid item xs={12} md={6}>
                    <EditableTypography
                      gutterBottom variant="h5" component="h3" onChange={(event) => this.handleOnChange(event)}
                      id="title" value={this.state.activity.title} editing={this.state.editing} />
                  </Grid>
                </Grid>
                {this.renderIf (!this.state.editing,
                  (<Grid container spacing={8}>
                     <Grid item><Typography color="primary" component="h6">Deltagare:</Typography></Grid>
                     {this.getAttendees()}
                   </Grid>))}
              </Grid>
              <Grid item xs={10} md={8}>
                <Grid container>
                  <Grid item xs>
                    <EditableTypography
                      variant="h6" component="h6" value={this.state.activity.description} multiline fullWidth
                      id="descripion" onChange={event => this.handleOnChange(event)} editing={this.state.editing} />
                  </Grid>
                </Grid>
              </Grid>
            </>
        );
    }

    renderIf(bool, content) {
        return bool ? content : "";
    }


    buttonSet() {
        if (this.state.editing) {
            return this.renderEditingButtons();
        } else {
            return this.renderNormalButtons();
        }
    }

    getAttendees() {
        let mapAttendees = (obj, func) => Utils.objectToList(Utils.objectMapWithKey(obj, func));
	let constructAttendee = (k, v) => <Grid item><Attendee component="p" person={v} key={k}/></Grid>;
	return mapAttendees(this.props.activity.attendees, constructAttendee);
    }

    renderCard(content) {
        return (
            <Card className="activity">
              <CardContent>
                {content}
              </CardContent>
              <CardActions>
                <Grid container direction="row" spacing={16} alignItems="center">
                  {this.buttonSet()}
                  <Grid item xs>{this.mailLink()}</Grid>
                </Grid>
              </CardActions>
            </Card>
        );
    }

    render() {
        let activity = this.state.activity;
	let content = (
            <>
              <Grid container justify="center" alignItems="baseline">
                {this.renderCardHeader()}
              </Grid>
              <Grid container justify="center">
                <Grid item xs={10} md={4}>
                  <Grid container alignItems="center" alignContent="center" justify="center">
                    <Grid item xs={4} sm={2}  md={5}><Typography component="h6">Ansvarig:</Typography></Grid>
                    <Grid item xs={8} sm={10} md={7}>
                      <EditableTypography component="p" value={this.props.isOwner ? 'Du' : activity.host.name}
                                          disabled editing={this.state.editing}/>
                    </Grid>
                  </Grid>
                </Grid>
                <Grid item xs={10} md={4}>
                  <Grid container>
                    <Grid item xs={4} sm={2}><Icon>today</Icon></Grid>
                    <Grid item xs={8} sm={10}>
                      <EditableTypography
                        component="p" value={this.state.activity.date} editing={this.state.editing}
                        handleOnChange={event => this.handleOnChange(event)} id="date" />
                    </Grid>
                  </Grid>
                </Grid>
                <Grid item xs={10} md={4}>
                  <Grid container>
		    <Grid item xs={4} sm={2}><Icon color="secondary">location_on</Icon></Grid>
	            <Grid item xs={8} sm={10}>
                      <EditableTypography
                        component="p" value={this.state.activity.meetingPoint} editing={this.state.editing}
                        handleOnChange={event => this.handleOnChange(event)} id="meetingPoint"/>
                    </Grid>
                  </Grid>
                </Grid>
              </Grid>
            </>
        );

        return this.renderCard(content);
    }

    mailLink() {
        let mailAddress;
        let text;
        if (this.props.isOwner) {
	    const flattenAttendees = (attendees, func) => Utils.objectToList(Utils.objectMap(attendees, func)).join(",");
	    mailAddress = flattenAttendees(this.props.activity.attendees, (attendee) => attendee.email);
            text = "Maila alla deltagare";
        } else {
            mailAddress = this.props.activity.host.email;
            text = "Maila ansvarig";
        }

        return <Button onClick={() => window.open('mailto:' + mailAddress, "_blank")}
                       color="primary" variant="outlined" size="small">{text} <Icon>email</Icon></Button>;
    }

    handleOnChange(event) {
        let activity = this.state.activity;
        activity[event.target.id] = event.target.value;
        this.setState({activity:activity});
    }

    /* Buttons */
    renderEditingButtons() {
        return (
            <>
              <Grid item xs>
                <Button variant="contained" color="primary"
                        onClick={() => this.props.saveActivity(this.state.activity)}>
                  Spara aktivitet <Icon>done</Icon>
                </Button>
              </Grid>
              <Grid item xs>{this.toggleEditingButton()}</Grid>
              <Grid item xs>{this.removeButton()}</Grid>
            </>
        );
    }

    renderNormalButtons() {
        if (this.props.isOwner) {
            return (
                <>
                  <Grid item xs>{this.toggleEditingButton()}</Grid>
                  <Grid item xs>{this.removeButton()}</Grid>
                </>
            );
        } else {
            return (
                <>
                  <Grid item xs>{this.attendenceButton()}</Grid>
                </>
            );
        }
    }

    attendenceButton() {
        return (
            <Button size="small" color="primary" variant="contained" onClick={() => this.props.toggleAttendence()}>
              {this.props.activity.attendees[this.props.user.email] ? "Avanmäl intresse" : "Anmäl intresse"}
            </Button>
        );
    }

    removeButton() {
        return (
            <Button size="small" variant="outlined" onClick={() => this.props.removeActivity()}
                    color="secondary">
              Ta bort aktivitet <DeleteIcon/>
            </Button>
        );
    }

    toggleEditingButton() {
        return (
            <Button size="small" variant="outlined" onClick={() => this.setState({editing: !this.state.editing})}>
              {this.state.editing ? 'Avbryt' : 'Ändra aktivitet'} <Icon>{this.state.editing ? 'cancel' : 'edit_icon'}</Icon>
            </Button>
        );
    }
}

function Attendee(props) {
    return <Typography color={props.color} component={props.component}>{props.person.name}</Typography>;
}

function EditableTypography(props) {
    if (!props.editing) {
        return (
            <Typography color={props.color} component={props.component} gutterBottom={props.gutterBottom}
                        variant={props.variant}>{props.value}</Typography>
        );
    } else {
        return (
            <TextField id={props.id} label={props.label} defaultValue={props.value} fullWidth={props.fullWidth}
                       onChange={props.onChange} multiline={props.multiline} disabled={props.disabled}/>
        );
    }
}

export default Activity;
