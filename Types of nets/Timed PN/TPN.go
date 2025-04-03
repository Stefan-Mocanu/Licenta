package main

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"os"
	"slices"
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
	VALUE int    `json:"value"`
}
type Transition struct {
	INPUT  []Arc `json:"input"`
	OUTPUT []Arc `json:"output"`
	TIME   int   `json:"TIME"` // ! Added TIME attribute
}
type Marking struct {
	CONTENT map[string]int `json:"content"`
}
type Net struct {
	PLACES      map[string]Place      `json:"places"`
	TRANSITIONS map[string]Transition `json:"transitions"`
}
type JSONInput struct {
	NET Net     `json:"net"`
	M0  Marking `json:"m0"`
}

func readNetFromJSONFile() (Net, Marking) {
	dataJson, err := os.ReadFile("net.json")
	check(err)
	var data JSONInput
	json.Unmarshal(dataJson, &data)
	// fmt.Println(data)
	var net Net
	var m0 Marking
	net = data.NET
	m0 = data.M0
	for place := range net.PLACES {
		_, ok := m0.CONTENT[place]
		if !ok {
			m0.CONTENT[place] = 0
		}
	}

	// ! 1 is the default value for TIME
	for t, _ := range net.TRANSITIONS {
		if net.TRANSITIONS[t].TIME < 1 {
			temp := net.TRANSITIONS[t]
			temp.TIME = 1
			net.TRANSITIONS[t] = temp
		}
	}

	return net, m0
}

func compareArc(marking Marking, arc Arc) bool {
	return marking.CONTENT[arc.PLACE] < arc.VALUE
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

	for _, arc := range transition.INPUT {
		newMarking.CONTENT[arc.PLACE] -= arc.VALUE
	}
	// for _, arc := range transition.OUTPUT {
	// 	newMarking.CONTENT[arc.PLACE] += arc.VALUE
	// }
	return newMarking
}

func endTransition(net Net, transitionName string, m Marking) Marking {
	newMarking := m
	transition := net.TRANSITIONS[transitionName]

	for _, arc := range transition.OUTPUT {
		newMarking.CONTENT[arc.PLACE] += arc.VALUE
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

	type tuple struct { // ! Added struct
		transition string
		time       int
		id         int
	}
	startId := 0
	globaltime := 0                // ! Added time counter
	activeTransitions := []tuple{} // ! added activeTranstions

	transitions = getViableTransitions(net, markings[0])
	for len(transitions) != 0 || len(activeTransitions) != 0 {
		globaltime++
		fmt.Println("TIME: ", globaltime)
		if len(transitions) > 0 { // ! Verify if there are viable transitions
			transition := selectTransition(transitions)
			current_marking = activateTransition(net, transition, markings[len(markings)-1])
			fmt.Println("Started transition", transition, "with id:", startId)
			markings = append(markings, current_marking)
			activeTransitions = append(activeTransitions, tuple{
				transition: transition,
				time:       net.TRANSITIONS[transition].TIME,
				id:         startId,
			}) // ! Added to activeTranstions
			startId++
		}

		// ! Added new logic
		deleted := 0
		copy_activeTransitions := activeTransitions
		for index, tuple := range copy_activeTransitions {
			if tuple.time == 1 {
				activeTransitions = slices.Delete(activeTransitions, index-deleted, index-deleted+1)
				current_marking = endTransition(net, tuple.transition, markings[len(markings)-1])
				markings = append(markings, current_marking)
				fmt.Println("Ended transtition", tuple.transition, "with id:", tuple.id)
				deleted++
			} else {
				activeTransitions[index-deleted].time--
			}
		}
		// ! End of new logic
		fmt.Println(markings[len(markings)-1])
		transitions = getViableTransitions(net, markings[len(markings)-1])
	}
	fmt.Println("There are no more viable transitions")
}
