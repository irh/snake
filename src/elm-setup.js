var elm = Elm.fullscreen(Elm.Main);

elm.ports.testPort.subscribe(function (data) {
  console.log(data);
});
