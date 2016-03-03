var soundPlayer = new Howl(soundSprite);

var elm = Elm.fullscreen(Elm.Main);

elm.ports.playSound.subscribe(function (sound) {
  soundPlayer.play(sound);
});

