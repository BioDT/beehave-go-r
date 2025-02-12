// Demonstrates how to collect data in memory.
package main

import (
	"C"
	"encoding/json"
	"fmt"
	"math"

	"github.com/mlange-42/arche/ecs"
	"github.com/mlange-42/beecs/model"
	"github.com/mlange-42/beecs/obs"
	"github.com/mlange-42/beecs/params"
)

// WorkerData represents the worker cohorts data in a tabular format
type WorkerData struct {
	Columns []string     `json:"columns"` // Column names including "tick"
	Data    [][]*float64 `json:"data"`    // 2D array of data, including tick as first column
}

// SimulationData represents all observer data
type SimulationData struct {
	WorkerCohorts *WorkerData `json:"worker_cohorts,omitempty"` // Data from worker cohorts observer
	AgeStructure  *WorkerData `json:"age_structure,omitempty"`  // Data from age structure observer
	Stores        *WorkerData `json:"stores,omitempty"`         // Data from stores observer
	PatchPollen   *WorkerData `json:"patch_pollen,omitempty"`   // Data from patch pollen observer
	PatchNectar   *WorkerData `json:"patch_nectar,omitempty"`   // Data from patch nectar observer
	NectarVisits  *WorkerData `json:"nectar_visits,omitempty"`  // Data from nectar visits observer
	PollenVisits  *WorkerData `json:"pollen_visits,omitempty"`  // Data from pollen visits observer
}

// MemoryReporter is a system that collects data from an observer in memory
type MemoryReporter struct {
	observer       obs.WorkerCohorts
	data           WorkerData
	tick           int
	MaxTimesteps   int // Maximum number of timesteps to keep. Zero means unlimited.
	UpdateInterval int // Interval for getting data, in model ticks. Optional.
}

// Initialize implements ecs.System
func (r *MemoryReporter) Initialize(w *ecs.World) {
	r.observer.Initialize(w)
	// Add "tick" as the first column
	columns := append([]string{"tick"}, r.observer.Header()...)
	r.data = WorkerData{
		Columns: columns,
		Data:    make([][]*float64, 0),
	}
}

// Update implements ecs.System
func (r *MemoryReporter) Update(w *ecs.World) {
	if r.UpdateInterval > 0 && r.tick%r.UpdateInterval != 0 {
		r.tick++
		return
	}

	// Get values from the observer
	values := r.observer.Values(w)

	// Create a new row with tick as the first value
	row := make([]*float64, len(r.data.Columns))
	tick := float64(r.tick)
	row[0] = &tick
	for i, v := range values {
		val := v
		row[i+1] = &val
	}

	// Add the row to the data
	r.data.Data = append(r.data.Data, row)

	r.tick++
}

// Finalize implements ecs.System
func (r *MemoryReporter) Finalize(w *ecs.World) {}

// AgeStructureReporter is a system that collects data from the AgeStructure observer
type AgeStructureReporter struct {
	observer       obs.AgeStructure
	data           WorkerData
	tick           int
	MaxTimesteps   int // Maximum number of timesteps to keep. Zero means unlimited.
	UpdateInterval int // Interval for getting data, in model ticks. Optional.
}

// Initialize implements ecs.System
func (r *AgeStructureReporter) Initialize(w *ecs.World) {
	r.observer.Initialize(w)
	// Add "tick" and "age" as the first columns
	columns := append([]string{"tick", "age"}, r.observer.Header()...)
	r.data = WorkerData{
		Columns: columns,
		Data:    make([][]*float64, 0),
	}
}

// Update implements ecs.System
func (r *AgeStructureReporter) Update(w *ecs.World) {
	if r.UpdateInterval > 0 && r.tick%r.UpdateInterval != 0 {
		r.tick++
		return
	}

	// Get values from the observer
	values := r.observer.Values(w)

	// For each age group, create a row with tick and age
	for age, ageValues := range values {
		row := make([]*float64, len(r.data.Columns))
		tick := float64(r.tick)
		ageFloat := float64(age)
		row[0] = &tick     // tick
		row[1] = &ageFloat // age

		// Copy values for this age group
		for i, v := range ageValues {
			if !math.IsNaN(v) {
				val := v
				row[i+2] = &val
			}
		}

		// Add the row to the data
		r.data.Data = append(r.data.Data, row)
	}

	r.tick++
}

// Finalize implements ecs.System
func (r *AgeStructureReporter) Finalize(w *ecs.World) {}

// StoresReporter is a system that collects data from the Stores observer
type StoresReporter struct {
	observer       obs.Stores
	data           WorkerData
	tick           int
	MaxTimesteps   int // Maximum number of timesteps to keep. Zero means unlimited.
	UpdateInterval int // Interval for getting data, in model ticks. Optional.
}

// Initialize implements ecs.System
func (r *StoresReporter) Initialize(w *ecs.World) {
	r.observer.Initialize(w)
	// Add "tick" as the first column
	columns := append([]string{"tick"}, r.observer.Header()...)
	r.data = WorkerData{
		Columns: columns,
		Data:    make([][]*float64, 0),
	}
}

// Update implements ecs.System
func (r *StoresReporter) Update(w *ecs.World) {
	if r.UpdateInterval > 0 && r.tick%r.UpdateInterval != 0 {
		r.tick++
		return
	}

	// Get values from the observer
	values := r.observer.Values(w)

	// Create a new row with tick as the first value
	row := make([]*float64, len(r.data.Columns))
	tick := float64(r.tick)
	row[0] = &tick
	for i, v := range values {
		if !math.IsNaN(v) {
			val := v
			row[i+1] = &val
		}
	}

	// Add the row to the data
	r.data.Data = append(r.data.Data, row)

	r.tick++
}

// Finalize implements ecs.System
func (r *StoresReporter) Finalize(w *ecs.World) {}

// PatchPollenReporter is a system that collects data from the PatchPollen observer
type PatchPollenReporter struct {
	observer       obs.PatchPollen
	data           WorkerData
	tick           int
	MaxTimesteps   int // Maximum number of timesteps to keep. Zero means unlimited.
	UpdateInterval int // Interval for getting data, in model ticks. Optional.
}

// Initialize implements ecs.System
func (r *PatchPollenReporter) Initialize(w *ecs.World) {
	r.observer.Initialize(w)
	// Add "tick" as the first column
	columns := append([]string{"tick"}, r.observer.Header()...)
	r.data = WorkerData{
		Columns: columns,
		Data:    make([][]*float64, 0),
	}
}

// Update implements ecs.System
func (r *PatchPollenReporter) Update(w *ecs.World) {
	if r.UpdateInterval > 0 && r.tick%r.UpdateInterval != 0 {
		r.tick++
		return
	}

	// Get values from the observer
	values := r.observer.Values(w)

	// Create a new row with tick as the first value
	row := make([]*float64, len(r.data.Columns))
	tick := float64(r.tick)
	row[0] = &tick
	for i, v := range values {
		if !math.IsNaN(v) {
			val := v
			row[i+1] = &val
		}
	}

	// Add the row to the data
	r.data.Data = append(r.data.Data, row)

	r.tick++
}

// Finalize implements ecs.System
func (r *PatchPollenReporter) Finalize(w *ecs.World) {}

// PatchNectarReporter is a system that collects data from the PatchNectar observer
type PatchNectarReporter struct {
	observer       obs.PatchNectar
	data           WorkerData
	tick           int
	MaxTimesteps   int // Maximum number of timesteps to keep. Zero means unlimited.
	UpdateInterval int // Interval for getting data, in model ticks. Optional.
}

// Initialize implements ecs.System
func (r *PatchNectarReporter) Initialize(w *ecs.World) {
	r.observer.Initialize(w)
	// Add "tick" as the first column
	columns := append([]string{"tick"}, r.observer.Header()...)
	r.data = WorkerData{
		Columns: columns,
		Data:    make([][]*float64, 0),
	}
}

// Update implements ecs.System
func (r *PatchNectarReporter) Update(w *ecs.World) {
	if r.UpdateInterval > 0 && r.tick%r.UpdateInterval != 0 {
		r.tick++
		return
	}

	// Get values from the observer
	values := r.observer.Values(w)

	// Create a new row with tick as the first value
	row := make([]*float64, len(r.data.Columns))
	tick := float64(r.tick)
	row[0] = &tick
	for i, v := range values {
		if !math.IsNaN(v) {
			val := v
			row[i+1] = &val
		}
	}

	// Add the row to the data
	r.data.Data = append(r.data.Data, row)

	r.tick++
}

// Finalize implements ecs.System
func (r *PatchNectarReporter) Finalize(w *ecs.World) {}

// NectarVisitsReporter is a system that collects data from the NectarVisits observer
type NectarVisitsReporter struct {
	observer       obs.NectarVisits
	data           WorkerData
	tick           int
	MaxTimesteps   int // Maximum number of timesteps to keep. Zero means unlimited.
	UpdateInterval int // Interval for getting data, in model ticks. Optional.
}

// Initialize implements ecs.System
func (r *NectarVisitsReporter) Initialize(w *ecs.World) {
	r.observer.Initialize(w)
	// Add "tick" as the first column
	columns := append([]string{"tick"}, r.observer.Header()...)
	r.data = WorkerData{
		Columns: columns,
		Data:    make([][]*float64, 0),
	}
}

// Update implements ecs.System
func (r *NectarVisitsReporter) Update(w *ecs.World) {
	if r.UpdateInterval > 0 && r.tick%r.UpdateInterval != 0 {
		r.tick++
		return
	}

	// Get values from the observer
	values := r.observer.Values(w)

	// Create a new row with tick as the first value
	row := make([]*float64, len(r.data.Columns))
	tick := float64(r.tick)
	row[0] = &tick
	for i, v := range values {
		if !math.IsNaN(v) {
			val := v
			row[i+1] = &val
		}
	}

	// Add the row to the data
	r.data.Data = append(r.data.Data, row)

	r.tick++
}

// Finalize implements ecs.System
func (r *NectarVisitsReporter) Finalize(w *ecs.World) {}

// PollenVisitsReporter is a system that collects data from the PollenVisits observer
type PollenVisitsReporter struct {
	observer       obs.PollenVisits
	data           WorkerData
	tick           int
	MaxTimesteps   int // Maximum number of timesteps to keep. Zero means unlimited.
	UpdateInterval int // Interval for getting data, in model ticks. Optional.
}

// Initialize implements ecs.System
func (r *PollenVisitsReporter) Initialize(w *ecs.World) {
	r.observer.Initialize(w)
	// Add "tick" as the first column
	columns := append([]string{"tick"}, r.observer.Header()...)
	r.data = WorkerData{
		Columns: columns,
		Data:    make([][]*float64, 0),
	}
}

// Update implements ecs.System
func (r *PollenVisitsReporter) Update(w *ecs.World) {
	if r.UpdateInterval > 0 && r.tick%r.UpdateInterval != 0 {
		r.tick++
		return
	}

	// Get values from the observer
	values := r.observer.Values(w)

	// Create a new row with tick as the first value
	row := make([]*float64, len(r.data.Columns))
	tick := float64(r.tick)
	row[0] = &tick
	for i, v := range values {
		if !math.IsNaN(v) {
			val := v
			row[i+1] = &val
		}
	}

	// Add the row to the data
	r.data.Data = append(r.data.Data, row)

	r.tick++
}

// Finalize implements ecs.System
func (r *PollenVisitsReporter) Finalize(w *ecs.World) {}

// runBeecs runs a simulation with the given parameters and returns the results as JSON.
//
//export runBeecs
func runBeecs(paramsJSON *C.char) *C.char {
	// Get the default parameters
	p := params.Default()

	// Parse input JSON to check for reporters configuration
	var inputData struct {
		Reporters []string `json:"reporters"`
	}
	if err := json.Unmarshal([]byte(C.GoString(paramsJSON)), &inputData); err != nil {
		fmt.Printf("Warning: could not parse reporters config: %v\n", err)
	}

	// Read JSON string into parameters
	if err := json.Unmarshal([]byte(C.GoString(paramsJSON)), &p); err != nil {
		fmt.Printf("Error unmarshaling parameters: %v\n", err)
		return C.CString("")
	}

	// Create a model with the default sub-models
	m := model.Default(&p, nil)
	if m == nil {
		fmt.Printf("Error: model creation failed\n")
		return C.CString("")
	}

	// Create reporters
	var workerReporter *MemoryReporter
	var ageReporter *AgeStructureReporter
	var storesReporter *StoresReporter
	var patchPollenReporter *PatchPollenReporter
	var patchNectarReporter *PatchNectarReporter
	var nectarVisitsReporter *NectarVisitsReporter
	var pollenVisitsReporter *PollenVisitsReporter

	// If no reporters specified, use all
	if len(inputData.Reporters) == 0 {
		workerReporter = &MemoryReporter{
			MaxTimesteps:   0,
			UpdateInterval: 1,
		}
		m.AddSystem(workerReporter)

		ageReporter = &AgeStructureReporter{
			MaxTimesteps:   0,
			UpdateInterval: 1,
		}
		m.AddSystem(ageReporter)

		storesReporter = &StoresReporter{
			MaxTimesteps:   0,
			UpdateInterval: 1,
		}
		m.AddSystem(storesReporter)

		patchPollenReporter = &PatchPollenReporter{
			MaxTimesteps:   0,
			UpdateInterval: 1,
		}
		m.AddSystem(patchPollenReporter)

		patchNectarReporter = &PatchNectarReporter{
			MaxTimesteps:   0,
			UpdateInterval: 1,
		}
		m.AddSystem(patchNectarReporter)

		nectarVisitsReporter = &NectarVisitsReporter{
			MaxTimesteps:   0,
			UpdateInterval: 1,
		}
		m.AddSystem(nectarVisitsReporter)

		pollenVisitsReporter = &PollenVisitsReporter{
			MaxTimesteps:   0,
			UpdateInterval: 1,
		}
		m.AddSystem(pollenVisitsReporter)
	} else {
		// Add only specified reporters
		for _, name := range inputData.Reporters {
			switch name {
			case "worker_cohorts":
				workerReporter = &MemoryReporter{
					MaxTimesteps:   0,
					UpdateInterval: 1,
				}
				m.AddSystem(workerReporter)
			case "age_structure":
				ageReporter = &AgeStructureReporter{
					MaxTimesteps:   0,
					UpdateInterval: 1,
				}
				m.AddSystem(ageReporter)
			case "stores":
				storesReporter = &StoresReporter{
					MaxTimesteps:   0,
					UpdateInterval: 1,
				}
				m.AddSystem(storesReporter)
			case "patch_pollen":
				patchPollenReporter = &PatchPollenReporter{
					MaxTimesteps:   0,
					UpdateInterval: 1,
				}
				m.AddSystem(patchPollenReporter)
			case "patch_nectar":
				patchNectarReporter = &PatchNectarReporter{
					MaxTimesteps:   0,
					UpdateInterval: 1,
				}
				m.AddSystem(patchNectarReporter)
			case "nectar_visits":
				nectarVisitsReporter = &NectarVisitsReporter{
					MaxTimesteps:   0,
					UpdateInterval: 1,
				}
				m.AddSystem(nectarVisitsReporter)
			case "pollen_visits":
				pollenVisitsReporter = &PollenVisitsReporter{
					MaxTimesteps:   0,
					UpdateInterval: 1,
				}
				m.AddSystem(pollenVisitsReporter)
			default:
				fmt.Printf("Warning: unknown reporter type: %s\n", name)
			}
		}
	}

	// Run the model
	m.Run()

	// Create simulation data structure, only including active reporters
	data := SimulationData{}
	if workerReporter != nil {
		data.WorkerCohorts = &workerReporter.data
	}
	if ageReporter != nil {
		data.AgeStructure = &ageReporter.data
	}
	if storesReporter != nil {
		data.Stores = &storesReporter.data
	}
	if patchPollenReporter != nil {
		data.PatchPollen = &patchPollenReporter.data
	}
	if patchNectarReporter != nil {
		data.PatchNectar = &patchNectarReporter.data
	}
	if nectarVisitsReporter != nil {
		data.NectarVisits = &nectarVisitsReporter.data
	}
	if pollenVisitsReporter != nil {
		data.PollenVisits = &pollenVisitsReporter.data
	}

	// Convert to JSON
	jsonData, err := json.Marshal(data)
	if err != nil {
		fmt.Printf("Error marshaling results: %v\n", err)
		return C.CString("")
	}

	return C.CString(string(jsonData))
}

func main() {
}
