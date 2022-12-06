// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"

	"github.com/ryukinix/adventofcode/lerax"
)

const windowPacketMarker int = 4
const windowMessageMarker int = 14

func PacketMarker(s string, window int) int {
	var begin, end int
	for i := 0; i < len(s); i++ {
		begin = i - window
		end = i
		if begin >= 0 && lerax.UniqueChars(s[begin:end]) {
			return i
		}
	}

	return -1
}

func main() {
	signals := lerax.ReadFileText("input.txt")
	fmt.Println(PacketMarker(signals, windowPacketMarker))
	fmt.Println(PacketMarker(signals, windowMessageMarker))
}
