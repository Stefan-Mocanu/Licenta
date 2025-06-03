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


type Arc struct {
	PLACE string `json:"place"`
	
	VALUE map[string]int `json:"value"`
	
	INHIBITOR map[string]int    `json:"inhibitor"`
}
type Transition struct {
	INPUT  []Arc `json:"input"`
	OUTPUT []Arc `json:"output"`
	MINTIME   int   `json:"minTime"`
	MAXTIME int   `json:"maxTime"`
}
type Marking struct {
	
	CONTENT map[string]map[string]int `json:"content"`
}
type Net struct {
	PLACES      []string     `json:"places"`
	TRANSITIONS map[string]Transition `json:"transitions"`
}
type JSONInput struct {
	NET Net     `json:"net"`
	M0  Marking `json:"m0"`
	COLORS []string `json:"colors"`
}

var colors = map[string]bool{}

func readNetFromJSONFile() (Net, Marking) {
	dataJson, err := os.ReadFile("net.json")
	check(err)
	var data JSONInput
	json.Unmarshal(dataJson, &data)
	var net Net
	var m0 Marking
	net = data.NET
	m0 = data.M0

	colors_temp := data.COLORS
	for _, color := range colors_temp {
		colors[color] = true
	}

	for _, place := range net.PLACES {
		_, ok := m0.CONTENT[place]
		if !ok {
			
			m0.CONTENT[place] = map[string]int{}
		}
		for color, _ := range colors {
			_, ok := m0.CONTENT[place][color]
			if !ok {
				m0.CONTENT[place][color] = 0
			}
		}
	}

	for t, _ := range net.TRANSITIONS {
		if net.TRANSITIONS[t].MINTIME < 1 {
			temp := net.TRANSITIONS[t]
			temp.MINTIME = 1
			net.TRANSITIONS[t] = temp
		}
		if net.TRANSITIONS[t].MAXTIME < 1 {
			temp := net.TRANSITIONS[t]
			temp.MAXTIME = 1
			net.TRANSITIONS[t] = temp
		}
	}

	return net, m0
}

func compareArc(marking Marking, arc Arc) bool {
	
	
	ok := false
	for color, _ := range colors {
		_, verify := arc.VALUE[color]
		if !verify {
			continue
		}
		if (marking.CONTENT[arc.PLACE][color] < arc.VALUE[color]) || (arc.INHIBITOR[color] < marking.CONTENT[arc.PLACE][color]) {
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
			if !compareArc(marking, arc) {
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
		
		for color, _ := range colors {
			newMarking.CONTENT[arc.PLACE][color] -= arc.VALUE[color]
		}
	}
	for _, arc := range transition.OUTPUT {
		
		 for color, _ := range colors {
			newMarking.CONTENT[arc.PLACE][color] += arc.VALUE[color]
		}
	}
	return newMarking
}



func endTransition(net Net, transitionName string, m Marking) Marking {
	newMarking := m
	transition := net.TRANSITIONS[transitionName]

	for _, arc := range transition.OUTPUT {
		for color, _ := range colors {
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


	type tuple struct {
		transition string
		time       int
		id         int
	}
	startId := 0
	globaltime := 0              
	activeTransitions := []tuple{}

	transitions = getViableTransitions(net, markings[0])
	for len(transitions) != 0 || len(activeTransitions) != 0{
		

		globaltime++
		fmt.Println("TIME: ", globaltime)
		if len(transitions) > 0 {
			transition := selectTransition(transitions)
			current_marking = activateTransition(net, transition, markings[len(markings)-1])
			fmt.Println("Started transition", transition, "with id:", startId)
			markings = append(markings, current_marking)
			mini := net.TRANSITIONS[transition].MINTIME
			maxi := net.TRANSITIONS[transition].MAXTIME
			var transition_time int
			if mini == maxi {
				transition_time = mini
			} else {
				transition_time = rand.Intn(maxi-mini) + mini
			}
			activeTransitions = append(activeTransitions, tuple{
				transition: transition,
				time:       transition_time,
				id:         startId,
			}) 
			startId++
		}

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
		fmt.Println(markings[len(markings)-1])
		transitions = getViableTransitions(net, markings[len(markings)-1])
	}
	fmt.Println("There are no more viable transitions")
}
