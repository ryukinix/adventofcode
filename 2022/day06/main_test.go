// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"testing"
)

func TestPacketMarker(t *testing.T) {
	tests := []struct {
		signals  string
		expected int
	}{
		{
			signals:  "bvwbjplbgvbhsrlpgdmjqwftvncz",
			expected: 5,
		},
		{
			signals:  "nppdvjthqldpwncqszvftbrmjlhg",
			expected: 6,
		},
		{
			signals:  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
			expected: 10,
		},
		{
			signals:  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",
			expected: 11,
		},
	}

	for _, test := range tests {
		got := PacketMarker(test.signals, windowPacketMarker)
		if got != test.expected {
			t.Errorf("For test=%v, expected %v, but got %v", test.signals, test.expected, got)
		}
	}
}
