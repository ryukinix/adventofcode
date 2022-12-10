// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"

	"github.com/ryukinix/adventofcode/lerax"
)

type File struct {
	name string
	size int
}

type Directory struct {
	name  string
	size  int
	dirs  []*Directory
	files []*File
	level int
}

type FileSystem struct {
	name string
	Directory
}

const MaxSize = 100000
const FileSystemSpace = 70000000
const UpgradeSpaceNecessary = 30000000

type ExprType int

const (
	CommandCDEnter ExprType = iota
	Skip
	CommandCDBack
	FileOutput
	UnknownExpression
)

func expressionType(tokens []string) ExprType {
	first := tokens[0]
	switch first {
	case "$":
		cmd := tokens[1]
		switch cmd {
		case "ls":
			return Skip
		case "cd":
			arg := tokens[2]
			if arg == ".." {
				return CommandCDBack
			} else if arg != "/" {
				return CommandCDEnter
			} else {
				return Skip
			}
		}

	case "dir":
		return Skip

	default:
		if lerax.IsInt(first) {
			return FileOutput
		}
	}

	return UnknownExpression
}

func (d *Directory) parseDirectory(commands []string) []string {
	for len(commands) > 0 {
		command := commands[0]
		commands = commands[1:]
		tokens := strings.Split(command, " ")
		switch expressionType(tokens) {
		case CommandCDBack:
			return commands
		case CommandCDEnter:
			dir := &Directory{
				name:  tokens[2],
				dirs:  []*Directory{},
				files: []*File{},
				level: d.level + 2,
			}
			commands = dir.parseDirectory(commands)
			d.addDirectory(dir)
		case FileOutput:
			value, _ := strconv.Atoi(tokens[0])
			f := &File{
				name: tokens[1],
				size: value,
			}
			d.addFile(f)

		case UnknownExpression:
			panic("Unknown Expression! The parser has a bug inside it, good luck")
		case Skip:
		}

	}
	return commands
}

func (d *Directory) addDirectory(dir *Directory) {
	d.dirs = append(d.dirs, dir)
}

func (d *Directory) addFile(f *File) {
	d.files = append(d.files, f)
}

func (d *Directory) calculateSize() int {
	if d.size != 0 {
		return d.size
	}
	size := 0
	for _, f := range d.files {
		size = size + f.size
	}
	for _, d := range d.dirs {
		size = size + d.calculateSize()
	}
	d.size = size
	return size
}

func (d *Directory) calculateSizeMax(max int) int {
	total := 0
	for _, dir := range d.dirs {
		if dir.size <= max {
			total = total + dir.size
		}
		total = total + dir.calculateSizeMax(max)
	}
	return total
}

func (d *Directory) printDirectoryTree() {
	r := " "
	if d.size <= MaxSize {
		r = "-"
	}
	s := strings.Repeat(r, d.level)
	fmt.Printf("|%s|== (dir=%d) %s\n", s, d.size, d.name)
	for _, f := range d.files {
		s := strings.Repeat(" ", d.level+1)
		fmt.Printf("|%s|== (file=%d) %s\n", s, f.size, f.name)
	}

	for _, dir := range d.dirs {
		dir.printDirectoryTree()
	}
}

func (d *Directory) collectDirsWithSizeAtLeast(minimumSize int) []*Directory {
	dirs := []*Directory{}
	for _, dir := range d.dirs {
		if dir.size >= minimumSize {
			dirs = append(dirs, dir)
		}
		dirs = append(dirs, dir.collectDirsWithSizeAtLeast(minimumSize)...)
	}
	return dirs
}

func (fs *FileSystem) resolvePart1() {
	result := fs.calculateSizeMax(MaxSize)
	fs.printDirectoryTree()
	fmt.Println("Max size sum: ", result)
}

func (fs *FileSystem) resolvePart2() {
	availableSpace := FileSystemSpace - fs.size
	toBeDeleted := UpgradeSpaceNecessary - availableSpace
	dirs := fs.collectDirsWithSizeAtLeast(toBeDeleted)
	println("To be deleted at least: ", toBeDeleted)
	sort.Slice(dirs, func(a, b int) bool {
		return dirs[a].size < dirs[b].size
	})
	d := dirs[0]
	fmt.Printf("Directory to be deleted: (%s, %d)", d.name, d.size)
}

func main() {
	readFile, err := os.Open("input.txt")
	lerax.ErrCheck(err)
	defer readFile.Close()

	commands := lerax.LoadLines(readFile)
	fs := &FileSystem{name: "/"}
	fs.parseDirectory(commands)
	fs.calculateSize()
	fs.resolvePart1()
	fs.resolvePart2()
}
