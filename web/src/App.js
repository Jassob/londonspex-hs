import React, { Component } from 'react';
import axios from 'axios';
import CssBaseline from '@material-ui/core/CssBaseline';
import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';

import './App.css';
import settings from './settings.js';
import Utils from './Utils.js';
import ActivityList from './ActivityList.js';
import LoginForm from './LoginForm.js';

class App extends Component {
    constructor(props) {
	super(props);

	this.state = {
	    user: JSON.parse(Utils.storageRead("user")),
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

    logout() {
	Utils.storageDelete("user");
	this.setState({"user": undefined});
    }

    render() {
	let content = this.state.user ? this.renderActivities() : this.renderLogin();
	let logoutButton = this.state.user ? <Button variant="contained" color="secondary"
                                                     onClick={() => this.logout()}>Logga ut</Button> : "";
	return (
	    <Grid container
                  justify="space-around">
              <CssBaseline />
              <Grid item xs={12} md={10}>
                <Grid container direction="row"
                      justify="center"
                      alignItems="baseline"
                      alignContent="center"
                      className="header" spacing={24}>
		  <Grid item xs={8} md={6}><Typography variant="h3">Aktiviteter - London</Typography></Grid>
		  <Grid item xs={8} md={6}><Typography variant="h4">Chalmersspexet Bob 2018 Geronimo</Typography></Grid>
	        </Grid>
                <Grid container alignItems="baseline">
                  <Grid item xs>
	            {content}
                  </Grid>
                </Grid>
                <hr/>
                <Grid container direction="row" spacing={16}>
                  <Grid item xs={4} md={2}>
                    {logoutButton}
                  </Grid>
                  <Grid item xs={10} md={6} className="left">
                    <User user={this.state.user}/>
                  </Grid>
                </Grid>
                <Grid className="footer" container justify="space-between" alignItems="baseline" spacing={24}>
                  <Grid item xs={3} md={2}><Typography>Skapad av <a href="mailto://jacob.t.jonsson@gmail.com">DGP</a></Typography></Grid>
                  <Grid item xs={3} md={1}><Typography>© 2018</Typography></Grid>
                </Grid>
	      </Grid>
            </Grid>
	);
    }

    renderLogin() {
	return (
	    <LoginForm
	      callback={(response) => {
		  Utils.storageStore("user", JSON.stringify(response));
		  this.setState({user: response});
	      }}
	      api={this.state.api} />
	);
    }

    renderActivities() {
	return (
	    <ActivityList
	      user={this.state.user}
	      api={this.state.api}
	      />
	);
    }
}

function User(props) {
    if (props.user) {
        return <Typography>Inloggad som {props.user.name} ({props.user.email})</Typography>;
    } else {
        return <Typography>Inte inloggad</Typography>;
    }

}

export default App;
