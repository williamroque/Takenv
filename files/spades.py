class Game():
    def __init__(self, players, bag_modulus, point_ceiling):
        self.players = players
        self.bag_modulus = bag_modulus
        self.point_ceiling = point_ceiling

        self.is_finished = False

        self.scores = {}
        for player in players:
            self.scores[player] = 0

        self.bets = {}

    def print_scores(self):
        for player, score in self.scores.items():
            print('{}: {}'.format(player, score))

    def set_bets(self):
        for player in self.players:
            self.bets[player] = abs(int(input('{}> '.format(player))))

    def set_tricks(self):
        if not self.is_finished:
            if len(self.bets.keys()) > 0:
                for player in self.players:
                    tricks = abs(int(input('{}> '.format(player))))
                    bet = self.bets[player]

                    if bet == 0:
                        if tricks == 0:
                            score = 100
                        else:
                            score = -100
                    else:
                        score = bet * 10 if tricks >= bet else 0
                        bags = max(0, tricks - bet) + self.scores[player] % self.bag_modulus

                        if bags >= self.bag_modulus:
                            score -= 100

                        score += max(0, tricks - bet)

                    self.scores[player] += score

                    if self.scores[player] >= self.point_ceiling:
                        self.is_finished = True

                self.bets = {}
            else:
                print('Set bets first.')

        print()
        self.print_scores()
