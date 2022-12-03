// Copyright 2022 the adventofcode Authors. All rights reserved
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

type Winner int

const (
	First  Winner = 0
	Second        = 6
	Draw          = 3
)

type Rule struct {
	hand1 Hand
	hand2 Hand
	win   Winner
}

var rules []Rule = []Rule{
	{
		hand1: Scissors,
		hand2: Rock,
		win:   Second,
	},

	{
		hand1: Scissors,
		hand2: Paper,
		win:   First,
	},

	{
		hand1: Paper,
		hand2: Rock,
		win:   First,
	},

	{
		hand1: Paper,
		hand2: Scissors,
		win:   Second,
	},

	{
		hand1: Rock,
		hand2: Scissors,
		win:   First,
	},

	{
		hand1: Rock,
		hand2: Paper,
		win:   Second,
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
		hand1: a,
		hand2: b,
		win:   Draw,
	}
}

func (r *Rule) equal(a, b Hand) bool {
	return a == r.hand1 && b == r.hand2
}

func (r *Rule) score() int {
	return int(r.hand2) + int(r.win)
}

func eval(games []string) int {
	score := 0
	hand := new(Hand)
	r := new(Rule)
	for _, game := range games {
		// fmt.Println("game: ", game)
		players := strings.Split(game, " ")
		p1, p2 := players[0], players[1]
		a, b := hand.fromString(p1), hand.fromString(p2)
		// fmt.Printf("Translate: %v, %v\n", a, b)
		rule := r.apply(a, b)
		// fmt.Println("Winner: ", rule.win)
		new_score := rule.score()
		// fmt.Printf("Score: %v\n", new_score)
		// fmt.Println()
		score = score + new_score
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
	defer func() {
		lerax.ErrCheck(readFile.Close())
	}()
	games := lerax.LoadLines(readFile)
	fmt.Println(eval(games))
}
