// Demonstrates how to collect data in memory.
package main

import (
	"C"
	"encoding/json"

	"github.com/mlange-42/arche/ecs"
	"github.com/mlange-42/beecs/model"
	"github.com/mlange-42/beecs/obs"
	"github.com/mlange-42/beecs/params"
)

// WorkerData represents the worker cohorts data in a tabular format
type WorkerData struct {
	Columns []string    `json:"columns"`   // Column names including "tick"
	Data    [][]float64 `json:"data"`      // 2D array of data, including tick as first column
}

// MemoryReporter is a system that collects data from an observer in memory
type MemoryReporter struct {
	observer       obs.WorkerCohorts
	data          WorkerData
	tick          int
	MaxTimesteps  int // Maximum number of timesteps to keep. Zero means unlimited.
	UpdateInterval int // Interval for getting data, in model ticks. Optional.
}

// Initialize implements ecs.System
func (r *MemoryReporter) Initialize(w *ecs.World) {
	r.observer.Initialize(w)
	// Add "tick" as the first column
	columns := append([]string{"tick"}, r.observer.Header()...)
	r.data = WorkerData{
		Columns: columns,
		Data:    make([][]float64, 0),
	}
}

// Update implements ecs.System
func (r *MemoryReporter) Update(w *ecs.World) {
	// Skip if not at update interval
	if r.UpdateInterval > 0 && r.tick%r.UpdateInterval != 0 {
		r.tick++
		return
	}

	r.observer.Update(w)
	values := r.observer.Values(w)

	// Add tick as the first value
	row := append([]float64{float64(r.tick)}, values...)
	r.data.Data = append(r.data.Data, row)

	// Limit the number of timesteps if MaxTimesteps is set
	if r.MaxTimesteps > 0 && len(r.data.Data) > r.MaxTimesteps {
		r.data.Data = r.data.Data[len(r.data.Data)-r.MaxTimesteps:]
	}

	r.tick++
}

// Finalize implements ecs.System
func (r *MemoryReporter) Finalize(w *ecs.World) {}

//export runBeecs
func runBeecs(paramsJSON *C.char) *C.char {
	// Get the default parameters
	p := params.Default()

	// Read JSON string from C to modify some parameters
	if err := json.Unmarshal([]byte(C.GoString(paramsJSON)), &p); err != nil {
		return C.CString("")
	}

	// Create a model with the default sub-models
	m := model.Default(&p, nil)

	// Create and initialize memory reporter
	reporter := &MemoryReporter{
		MaxTimesteps: 0,  // Keep all timesteps
		UpdateInterval: 1, // Collect every tick
	}

	// Add reporter directly as a system to the model
	m.AddSystem(reporter)

	// Run the model
	m.Run()

	// Convert data to JSON
	jsonData, err := json.Marshal(reporter.data)
	if err != nil {
		return C.CString("")
	}

	// Return JSON string
	return C.CString(string(jsonData))
}

func main() {
}
