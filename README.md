express-browser-router
======================

Source from: https://github.com/visionmedia/express

## Description

This was just an experiment, I took the router component out of the express and made it work inside a browser.

Tested (not much really) on Chromium and Firefox.

Originally I wanted to use it in a module I'm writing in my spare time, but I changed my plans.

Hope it will be useful to someone else

## Quick Start

Instantiate a new Router:

    var app = new Router();
    
Add routes:

    app.get('/', function(req, res, next){ 
        console.log('Index'); 
    });
    
Replace `a` click and `form` submit callbacks with something which create a req object and pass it to the router:

    app.middleware({ method: 'get', url: '/' });`

Check out the examples and the express documentation (and code: [/express/lib/router](https://github.com/visionmedia/express/tree/master/lib/router)) to see how you can use it.
