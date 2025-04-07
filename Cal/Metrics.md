# 📊 Temporal Network Metric Descriptions

This panel provides definitions and ecological interpretations for the temporal network metrics used in the Phenological Influence Explorer, based on the framework by Yin & Rudolf (2024).

---

## 🌐 Core Metrics

### **Downstream Set**
- **Definition:** Number of species that can be reached from a focal species via time-respecting paths.
- **Interpretation:** Indicates which species could potentially be influenced by the focal species, including indirect temporal pathways.

### **Upstream Set**
- **Definition:** Number of species that can reach a focal species via time-respecting paths.
- **Interpretation:** Reflects which species could influence the focal species, based on past flowering events.

### **Average Temporal Distance (Latency)**
- **Definition:** The average number of steps (or hops) along shortest time-respecting paths to other species.
- **Interpretation:** Measures how many links it takes to spread influence or a signal through the network.

### **Temporal Closeness Centrality**
- **Definition:** The inverse of the average temporal distance.
- **Interpretation:** Indicates how quickly a species can reach all others in a time-respecting manner.

---

## ⏳ Advanced Metrics (Available in Future Versions)

### **Time-Respecting Paths**
- **Definition:** Sequences of edges where each subsequent edge is activated later in time than the previous.
- **Use:** Ensures causality in the network.

### **Latency**
- **Definition:** The absolute time delay between events for species pairs.
- **Use:** Helps evaluate how quickly signals (e.g., pollinators, climate effects) propagate.

### **Burstiness**
- **Definition:** Variation in the timing of repeated events across species.
- **Use:** Indicates clustering or evenness of events over time.

### **Memory**
- **Definition:** Correlation between consecutive inter-event intervals.
- **Use:** Measures predictability in event timing.

---

### 📘 Reference
Yin, J. & Rudolf, V.H.W. (2024). Temporal networks in ecology: Uncovering structure and dynamics in seasonal communities. *Ecology Letters*, 27(7), 1361–1379.

