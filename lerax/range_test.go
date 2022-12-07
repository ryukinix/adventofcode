// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package lerax

import (
	"reflect"
	"testing"
)

func TestParseRange(t *testing.T) {
	got := ParseRange("2-10")
	expected := Range{min: 2, max: 10}
	if !reflect.DeepEqual(got, expected) {
		t.Errorf("Expected %q, got: %q", expected, got)
	}
}

func TestRangeOverlapsTrue(t *testing.T) {
	r1, r2 := NewRange(1, 10), NewRange(3, 5)
	got := r1.Overlaps(r2)
	expected := true
	if !reflect.DeepEqual(got, expected) {
		t.Errorf("Expected %v, got: %v", expected, got)
	}
}

func TestRangeOverlapsFalse(t *testing.T) {
	r1, r2 := NewRange(1, 3), NewRange(3, 5)
	got := r1.Overlaps(r2)
	expected := false
	if !reflect.DeepEqual(got, expected) {
		t.Errorf("Expected %v, got: %v", expected, got)
	}
}

func TestRangeOverlapsPartialTrue(t *testing.T) {
	r1, r2 := NewRange(5, 7), NewRange(3, 5)
	got := r1.OverlapsPartial(r2)
	expected := true
	if !reflect.DeepEqual(got, expected) {
		t.Errorf("Expected %v, got: %v", expected, got)
	}
}

func TestRangeOverlapsPartialFalse(t *testing.T) {
	r1, r2 := NewRange(2, 4), NewRange(5, 7)
	got := r1.OverlapsPartial(r2)
	expected := false
	if !reflect.DeepEqual(got, expected) {
		t.Errorf("Expected %v, got: %v", expected, got)
	}
}
