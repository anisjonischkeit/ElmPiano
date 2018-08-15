const tones = {};

app.ports.playStringNote.subscribe(note => {
  const snd = new Audio("./audio/" + note + ".wav");
  tones[note] = snd;

  snd.play();
});

app.ports.stopStringNote.subscribe(note => {
  let snd = tones[note];
  if (snd) {
    snd.pause();
  }
});
