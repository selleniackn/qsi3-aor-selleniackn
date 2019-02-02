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


let scoreWhenDeuce: player => score = winner => Advantage(winner);
let scoreWhenAdvantage: (player, player) => score =
  (advantagedPlayer, winner) =>
    advantagedPlayer == winner ? Game(winner) : Deuce;
/* This time we infer that the function type is (player) => player */
let other = player =>
  switch player {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };
let scoreWhenForty = (current, winner) => Game(winner);
let scoreWhenForty = (current, winner) =>
  current.player == winner ? Game(winner) : Deuce;
/* We add a tool function to increment point */
let incrementPoint: point => option(point) =
  point =>
    switch point {
    | Love => Some(Fifteen)
    | Fifteen => Some(Thirty)
    | Thirty => None
    };

let scoreWhenForty = (current, winner) =>
  current.player == winner ?
    Game(winner) :
    (
      switch (incrementPoint(current.otherPlayerPoint)) {
      | Some(p) => Forty({...current, otherPlayerPoint: p})
      | None => Deuce
      }
    );
let pointTo = (player, point, current) =>
  switch player {
  | PlayerOne => {...current, playerOne: point}
  | PlayerTwo => {...current, playerTwo: point}
  };

let pointFor = (player, current) =>
  switch player {
  | PlayerOne => current.playerOne
  | PlayerTwo => current.playerTwo
  };

let scoreWhenPoints = (current, winner) =>
  switch (current |> pointFor(winner) |> incrementPoint) {
  | Some(np) => Points(pointTo(winner, np, current))
  | None =>
    Forty({
      player: winner,
      otherPlayerPoint: current |> pointFor(other(winner))
    })
  };
let scoreWhenGame = winner => Game(winner);
let newGame = Points({playerOne: Love, playerTwo: Love});

let score = (current, winner) =>
  switch current {
  | Points(p) => scoreWhenPoints(p, winner)
  | Forty(f) => scoreWhenForty(f, winner)
  | Deuce => scoreWhenDeuce(winner)
  | Advantage(a) => scoreWhenAdvantage(a, winner)
  | Game(g) => scoreWhenGame(g)
  };
  let scoreWhenGame = winner => Game(winner);
  let newGame = Points({playerOne: Love, playerTwo: Love});

  let string_of_player= player => switch (player) {  
| PlayerOne => "PlayerOne"
| PlayerTwo => "PlayerTwo";
  };
  
  print_endline(string_of_player(PlayerOne));
  let string_of_point= point => switch(point){
| Love => "Love"
| Fifteen => "Fifteen"
| Thirty => "Thirty";

  };
  print_endline(string_of_point(Love));

  let string_of_score= score => switch(score){
  | Points(pts) => string_of_point(pts.playerOne) ++ "-" ++ string_of_point(pts.playerTwo)
  | Forty(fd) => fd.player == PlayerOne ? "Forty-" ++ string_of_point(fd.otherPlayerPoint): string_of_point(fd.otherPlayerPoint) ++ "-Forty"
  | Deuce => "Deuce"
  | Advantage(p) => "Advantage " ++ string_of_player(p)
  | Game(p) => "Game " ++ string_of_player(p);
    };
    print_endline(string_of_score(Forty{player: PlayerOne, otherPlayerPoint:Thirty }));
    print_endline(string_of_score(Deuce));
    print_endline(string_of_score(Advantage(PlayerOne)));
    print_endline(string_of_score(Game(PlayerOne)));
    print_endline(string_of_score(Points{playerOne: Love,playerTwo:Thirty }));


