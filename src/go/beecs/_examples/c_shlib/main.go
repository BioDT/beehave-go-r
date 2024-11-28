// Demonstrates how to write CSV output.
package main

import (
	"C"
	// "fmt"
	// "os"

	"github.com/mlange-42/arche-model/reporter"
	"github.com/mlange-42/beecs/model"
	"github.com/mlange-42/beecs/obs"
	"github.com/mlange-42/beecs/params"
)
import "log"

//export runbeecs
func runbeecs(str_params *C.char) int {
	inputJSON := C.GoString(str_params)

	// Get the default parameters.
	p := params.Default()
	// Read JSON string from C to modify some parameters.
	err := p.FromJSON([]byte(inputJSON))
	if err != nil {
		log.Fatal(err)
	}

	// Write the content of JSON passed from R.
	// file, err := os.Create("gotest.txt")
	// if err != nil {
	// 	panic(err)
	// }

	// out, err := fmt.Fprintf(file, "%s\n%+v", inputJSON, p.ForagingPeriod)
	// if err != nil {
	// 	panic(err)
	// }

	// if err := file.Close(); err != nil {
	// 	panic(err)
	// }

	// Create a model with the default sub-models.
	m := model.Default(&p, nil)

	// Add a CSV output system using observer [obs.WorkerCohorts].
	m.AddSystem(&reporter.CSV{
		Observer: &obs.WorkerCohorts{},
		File:     "out/worker-cohorts.csv",
	})

	// Add a CSV snapshot output system using observer [obs.ForagingStats].
	m.AddSystem(&reporter.SnapshotCSV{
		Observer:    &obs.ForagingStats{},
		FilePattern: "out/foraging-%04d.csv",
	})

	// Run the model.
	m.Run()

	return 0 //out
}

func main() {

}
