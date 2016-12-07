require('./main.css');

var Elm = require('./Main.elm');

var root = document.getElementById('root');

var session = null;
if (window.localStorage) {
  try {
    session = JSON.parse(
      window.localStorage.getItem('whistlet:session')
    );
  } catch (e) {
    console.log('Error decoding session information', e);
  }
}

var main = Elm.Main.embed(root, {
  session: session
});

main.ports.saveSession.subscribe(function (session) {
  console.log(session);
  if (window.localStorage) {
    try {
      window.localStorage.setItem('whistlet:session',
        JSON.stringify(session)
      );
    } catch (e) {
      console.log('Error saving session information', e);
    }
  }
});

main.ports.logout.subscribe(function () {
  if (window.localStorage) {
    try {
      window.localStorage.removeItem('whistlet:session');
    } finally {
      window.location.reload();
    }
  }
})
