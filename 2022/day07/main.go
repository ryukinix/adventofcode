// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

type File struct {
	name string
	path string
	size int
}

type Directory struct {
	name  string
	path  string
	dirs  []*Directory
	files []*File
	size  int
}

type FileSystem Directory

func ParseFileSystem(commands string) *FileSystem {
	return &FileSystem{
		name:  "adventofcodeday07",
		path:  "/",
		dirs:  []*Directory{},
		files: []*File{},
	}
}

func (d *Directory) addDirectory(dir *Directory) {
	d.dirs = append(d.dirs, dir)
}

func (d *Directory) addFile(f *File) {
	d.files = append(d.files, f)
}

func (d *Directory) calcSize() {

}
func (fs *FileSystem) resolvePart1() {

}

func (fs *FileSystem) resolvePart2() {

}

func main() {

}
