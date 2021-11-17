package main

import (
	"bufio"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
)

func pad(lines []string, max int) []string {
	for i, l := range lines {
		diff := max - len(l)
		if diff <= 0 {
			continue
		}
		l += strings.Repeat(" ", diff)
		lines[i] = l
	}
	return lines
}

func padOneFile(filename string) error {
	f, err := os.OpenFile(filename, os.O_RDWR, 0644)
	if err != nil {
		log.Fatal(err)
	}

	var lines []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		return errors.Wrapf(err, "Failed to scan %v", f.Name())
	}

	var max int
	for _, l := range lines {
		if len(l) > max {
			max = len(l)
		}
	}

	// pad
	lines = pad(lines, max)

	// write
	if _, err := f.Seek(0, 0); err != nil {
		return errors.Wrapf(err, "Seek(0,0) failed on %v", f.Name())
	}

	for _, l := range lines {
		f.WriteString(l)
		f.WriteString("\n")
	}
	f.Close()
	return nil
}

func main() {
	arg1 := os.Args[1]
	info, err := os.Stat(arg1)
	if os.IsNotExist(err) {
		log.Fatalf("%v does not exist", os.Args[1])
	}
	if info.IsDir() {
		matches, err := filepath.Glob(filepath.Join(arg1, "*.txt"))
		if err != nil {
			log.Fatal(err)
		}
		var errs []error
		for _, filename := range matches {
			if err := padOneFile(filename); err != nil {
				errs = append(errs, err)
			}
		}
		if len(errs) != 0 {
			for _, err := range errs {
				log.Println(err.Error())
			}
			os.Exit(1)
		}
	} else {
		filename := arg1
		if err := padOneFile(filename); err != nil {
			log.Fatal(err)
		}
	}

}
