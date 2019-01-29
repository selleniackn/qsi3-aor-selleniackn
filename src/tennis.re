type player =
  | PlayerOne
  | PlayerTwo;

type point =
  | Love
  | Fifteen
  | Thirty;

type pointsData = {
  playerOne: point,
  playerTwo: point
};

type fortyData = {
  player: player, /* The player who have forty points */
  otherPlayerPoint: point
};
let fd : fortyData = {player: PlayerOne, otherPlayerPoint: Love};

type score =
| Points(pointsData)
| Forty(fortyData)
| Deuce
| Advantage(player)
| Game(player);

let startScore : score = Points({playerOne: Love, playerTwo: Love});

let anotherScore : score = Forty({player: PlayerTwo, otherPlayerPoint:Thirty });

/* All exemple above don't work */
let impossibleScore1 : score = Points({playerOne: Seven, playerTwo: Eleven});
let impossibleScore2 : score = Points({playerOne: Forty, playerTwo: Forty});
let impossibleScore3 : score = Forty({player: PlayerTwo, otherPlayerPoint:Forty });