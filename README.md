# Event Sourcing Demo
[Live demo available here](https://joshsmithcode.github.io/event-source/)

This demo exists as a simple example of the possibilities of event sourcing. Using the forms below you can create events, then in the tables underneath you can see how we're able to create different 'tables' or 'visualisations' of your events. There's also a handy time-traveller that will allow you to move forward and backwards through events to watch how your data has changed over time.

There are a few assumptions made here to keep the examples simple. e.g. once a task is assigned, it's to the death, no removal or reassignment (yet).

To build:
`elm make src/Main.elm --output "./public/build/app.js"`

To run: 
Just open `./public/index.html` in your browser of choice
