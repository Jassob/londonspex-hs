import React, { Component } from 'react';
import Grid from '@material-ui/core/Grid';
import Button from '@material-ui/core/Button';
import TextField from '@material-ui/core/TextField';
import Typography from '@material-ui/core/Typography';

import './root.css';
import './LoginForm.css';
import Utils from './Utils.js';

class LoginForm extends Component {
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
	let errItems = this.state.errors.map((err, k) => <Grid item className="error" xs={8}>{err}</Grid>);
	let content = this.state.showRegister ? this.renderRegister() : this.renderLogin();
	return (
            <>
	      <Grid container spacing={8}>
	        {errItems}
	      </Grid>
	      {content}
            </>
	);
    }

    renderLogin() {
	return (
            <form validate>
              <br/>
              <Grid container justify="center">
                <Grid item xs={6}><Typography variant="h5" component="h3">Välkommen, var god logga in</Typography></Grid>
              </Grid>
              <Grid container justify="space-around" alignItems="center">
                <Grid item xs={8} md={4}>
                  <TextField id="email" type="text" required label="E-postaddress"
	                     onChange={(event) => this.handleFormUpdate(event)} />
                </Grid>
                <Grid item xs={8} md={4}>
                  <TextField id="password" type="password" required label="Lösendord"
                             onChange={(event) => this.handleFormUpdate(event)} />
                </Grid>
                <Grid item xs={8} md={4}>
                  <Button color="primary" variant="contained" type="button"
                          className="center" onClick={() => this.checkLogin()}>Logga in</Button>
                </Grid>
              </Grid>
              <br/>
              <Grid container spacing={8} justify="space-between">
	        <Grid item xs={6}><Typography component="p" variant="body2">Har du inget konto?</Typography></Grid>
	        <Grid item xs={4}>
                  <Button size="small" variant="outlined" onClick={() => this.setState({showRegister: true})}>Registrera</Button>
                </Grid>
              </Grid>
	    </form>
	);
    }

    renderRegister() {
	return (
            <form>
              <br/>
              <Grid container justify="center" alignContent="center" alignItems="baseline" spacing={8}>
                <Grid item xs={8} md={6}>
                  <Grid container alignContent="center">
                    <Grid item xs={12} md={7}>
                      <Typography variant="h5" component="h3">Välkommen, var god fyll i detaljerna nedan</Typography>
                    </Grid>
                    <Grid item xs={8} md={6}>
	              <TextField id="name" type="text" label="Namn" required
                                 onChange={(event) => this.handleFormUpdate(event)} />
	            </Grid>
                  </Grid>
                  <Grid container>
	            <Grid item xs={8} md={10}>
                      <TextField id="email" type="email" label="E-postaddress" required
                                 onChange={(event) => this.handleFormUpdate(event)} />
                    </Grid>
	          </Grid>
                  <Grid container spacing={8}>
	            <Grid item xs={8} md={10}>
                      <TextField name="password" type="password" label="Lösenord" required
		             onChange={(event) => this.handleFormUpdate(event)} />
                    </Grid>
                  </Grid>
                  <Grid container spacing={16}>
	            <Grid item xs={12}>
                      <Button color="primary" variant="contained" onClick={() => this.checkRegister()}>
                        Registrera
                      </Button>
                    </Grid>
                  </Grid>
                </Grid>
                <Grid item xs={8} md={5}>
                  <Typography variant="body2" component="p">Genom att skapa ett konto godkänner du att datan lagras så länge spexet är i London och max en vecka till. Datan du lämnar kommer endast att användas för att du ska kunna logga in på sidan.</Typography>
                </Grid>
              </Grid>
              <br />
              <hr/>
              <Grid container spacing={8} justify="space-around">
                <Grid item>
                  <Typography components="p" variant="body2">Redan registrerad?</Typography>
                </Grid>
                <Grid item>
	          <Button size="small" variant="outlined"
                          onClick={() => this.setState({showRegister: false})}>Logga in
                  </Button>
                </Grid>
              </Grid>
	    </form>
	);
    }

    checkLogin() {
	let payload = Utils.urlencode({ loginEmail: this.state.email, loginPassword: this.state.password });
	this.props.api.post('/login', payload)
	    .then((response) => this.props.callback(response.data))
	    .catch((error) => console.log(this.state.errors, error));
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

export default LoginForm;
