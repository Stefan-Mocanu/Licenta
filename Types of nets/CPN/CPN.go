package main

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"os"
)

func check(err error) {
	if err != nil {
		panic(err)
	}
}

type Place struct {
	PLACEHOLDER int `json:"placeholder"`
}

type Arc struct {
	PLACE string `json:"place"`
	// ! CHANGED VALUE Type
	VALUE map[string]int `json:"value"`
}
type Transition struct {
	INPUT  []Arc `json:"input"`
	OUTPUT []Arc `json:"output"`
}
type Marking struct {
	// ! Changed CONTENT Type
	CONTENT map[string]map[string]int `json:"content"`
}
type Net struct {
	PLACES      map[string]Place      `json:"places"`
	TRANSITIONS map[string]Transition `json:"transitions"`
}
type JSONInput struct {
	NET Net     `json:"net"`
	M0  Marking `json:"m0"`
	// ! Added COLORS
	COLORS []string `json:"colors"`
}

// ! Added Colors
var colors = map[string]bool{}

func readNetFromJSONFile() (Net, Marking) {
	dataJson, err := os.ReadFile("net.json")
	check(err)
	var data JSONInput
	json.Unmarshal(dataJson, &data)
	//fmt.Println(data)
	var net Net
	var m0 Marking
	net = data.NET
	m0 = data.M0

	// ! Added populating colors
	colors_temp := data.COLORS
	for _, color := range colors_temp {
		colors[color] = true
	}
	// ! End change

	for place := range net.PLACES {
		_, ok := m0.CONTENT[place]
		if !ok {
			m0.CONTENT[place] = map[string]int{} // ! Changed 0 to map[string]int{}
		}
		// ! Changed checking incomplete marking
		for color, _ := range colors {
			_, ok := m0.CONTENT[place][color]
			if !ok {
				m0.CONTENT[place][color] = 0
			}
		}
		// ! End Change
	}
	return net, m0
}

// ! Changed Function Behavior
func compareArc(marking Marking, arc Arc) bool {
	ok := false
	for color, _ := range colors {
		_, verify := arc.VALUE[color]
		if !verify {
			continue
		}
		if marking.CONTENT[arc.PLACE][color] < arc.VALUE[color] {
			ok = true
			break
		}
	}
	return ok
}

func getViableTransitions(net Net, marking Marking) []string {
	var result []string
	for transition := range net.TRANSITIONS {
		ok := true
		for _, arc := range net.TRANSITIONS[transition].INPUT {
			if compareArc(marking, arc) {
				ok = false
				break
			}
		}
		if ok {
			result = append(result, transition)
		}
	}
	return result
}
func selectTransition(transitions []string) string {
	randomIndex := rand.Intn(len(transitions))
	return transitions[randomIndex]
}

func activateTransition(net Net, transitionName string, m Marking) Marking {
	newMarking := m
	transition := net.TRANSITIONS[transitionName]

	for _, arc := range transition.INPUT { // ! Added FOR loop for colors
		for color, _ := range colors {
			newMarking.CONTENT[arc.PLACE][color] -= arc.VALUE[color]
		}
	}
	for _, arc := range transition.OUTPUT {
		for color, _ := range colors { // ! Added FOR loop for colors
			newMarking.CONTENT[arc.PLACE][color] += arc.VALUE[color]
		}
	}
	return newMarking
}
func main() {
	var net Net
	var markings []Marking
	var current_marking Marking
	net, current_marking = readNetFromJSONFile()
	markings = append(markings, current_marking)
	fmt.Println(current_marking)
	var transitions []string
	transitions = getViableTransitions(net, markings[0])
	for len(transitions) != 0 {
		transition := selectTransition(transitions)
		current_marking = activateTransition(net, transition, markings[len(markings)-1])
		markings = append(markings, current_marking)
		fmt.Println(markings[len(markings)-1])
		transitions = getViableTransitions(net, markings[len(markings)-1])
	}
	fmt.Println("There are no more viable transitions")
}
