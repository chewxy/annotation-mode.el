package main

import (
	"bufio"
	"log"
	"os"
	"strings"
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

func main() {
	filename := os.Args[1]
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
		log.Fatal(err)
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
		log.Fatal(f)
	}
	for _, l := range lines {
		f.WriteString(l)
		f.WriteString("\n")
	}
	f.Close()

}
