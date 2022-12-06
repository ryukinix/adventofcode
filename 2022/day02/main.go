// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/ryukinix/adventofcode/lerax"
)

type Hand int

const (
	Rock     Hand = 1
	Paper         = 2
	Scissors      = 3
)

type GameStatus int

const (
	Lose    GameStatus = 0
	Victory            = 6
	Draw               = 3
)

type Rule struct {
	hand1  Hand
	hand2  Hand
	status GameStatus
}

var rules []Rule = []Rule{
	{
		hand1:  Scissors,
		hand2:  Rock,
		status: Victory,
	},

	{
		hand1:  Scissors,
		hand2:  Paper,
		status: Lose,
	},

	{
		hand1:  Paper,
		hand2:  Rock,
		status: Lose,
	},

	{
		hand1:  Paper,
		hand2:  Scissors,
		status: Victory,
	},

	{
		hand1:  Rock,
		hand2:  Scissors,
		status: Lose,
	},

	{
		hand1:  Rock,
		hand2:  Paper,
		status: Victory,
	},
}

func (h Hand) fromString(str string) Hand {
	switch str {
	case "A", "X":
		return Rock
	case "B", "Y":
		return Paper
	case "C", "Z":
		return Scissors
	}
	// D:
	panic("everything is wrong if the code reaches here")
}

func (r Rule) apply(a, b Hand) Rule {
	for _, rule := range rules {
		if rule.equal(a, b) {
			return rule
		}
	}
	return Rule{
		hand1:  a,
		hand2:  b,
		status: Draw,
	}
}

func (r *Rule) equal(a, b Hand) bool {
	return a == r.hand1 && b == r.hand2
}

func (r *Rule) score() int {
	return int(r.hand2) + int(r.status)
}

func selectBestHand(hand Hand, status GameStatus) Hand {
	var bestHands = map[GameStatus]map[Hand]Hand{
		Draw: {
			Rock:     Rock,
			Scissors: Scissors,
			Paper:    Paper,
		},
		Lose: {
			Rock:     Scissors,
			Scissors: Paper,
			Paper:    Rock,
		},
		Victory: {
			Rock:     Paper,
			Scissors: Rock,
			Paper:    Scissors,
		},
	}
	return bestHands[status][hand]
}

func (r Rule) part2TransformGame() Rule {
	switch r.hand2 {
	case Rock:
		r.status = Lose
	case Paper:
		r.status = Draw
	case Scissors:
		r.status = Victory
	}
	r.hand2 = selectBestHand(r.hand1, r.status)
	return r
}

func eval(games []string, part2 bool) int {
	score := 0
	hand := new(Hand)
	for _, game := range games {
		// fmt.Println("game: ", game)
		players := strings.Split(game, " ")
		p1, p2 := players[0], players[1]
		a, b := hand.fromString(p1), hand.fromString(p2)
		// fmt.Printf("Translate: %v, %v\n", a, b)
		rule := Rule{
			hand1: a,
			hand2: b,
		}
		if part2 {
			rule = rule.part2TransformGame()
		} else {
			rule = rule.apply(a, b)
		}
		// fmt.Println("GameStatus: ", rule.status)
		newScore := rule.score()
		// fmt.Printf("Score: %v\n", new_score)
		// fmt.Println()
		score = score + newScore
	}
	return score
}

// debug stuff
func (h Hand) String() string {
	return []string{"", "Rock", "Paper", "Scissors"}[h]
}

func main() {
	readFile, err := os.Open("input.txt")
	lerax.ErrCheck(err)
	defer readFile.Close()

	games := lerax.LoadLines(readFile)
	// part1
	fmt.Println(eval(games, false))
	// part2
	fmt.Println(eval(games, true))
}
