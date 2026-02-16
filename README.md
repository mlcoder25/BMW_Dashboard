# BMW Sales Analysis Dashboard (2010–2024)

An interactive, high-performance R Shiny application designed to visualize and analyze BMW sales data globally. This dashboard provides a comprehensive view of market performance through dynamic filtering, key performance indicators (KPIs), and trend analysis.

---

## 🚀 Features

### **1. Interactive Dashboard**

* **Real-time KPIs:** Instantly view Total Sales Volume, Average Vehicle Price (USD), and the Top Selling Model based on your current filters.
* **Visual Analytics:** * **Top Models:** Horizontal bar charts showing the highest-performing models.
* **Fuel Type Share:** Pie charts illustrating the market distribution of Petrol, Diesel, Electric, and Hybrid vehicles.
* **Regional Performance:** Breakdown of sales across different global markets.



### **2. Advanced Trend Analysis**

* **Dynamic Time-Series:** Track the sales trajectory of the "Top N" models over a 14-year period (2010–2024).
* **Customizable View:** Use the "Top N" slider to adjust the number of models displayed in the trend plots for clearer comparative analysis.

### **3. Smart Data Management**

* **Cascading Filters:** The sidebar filters (Year, Region, Fuel Type, Transmission, etc.) are reactive. Selecting a "Region" will automatically update the "Model" dropdown to show only relevant choices available in that area.
* **Data Exploration:** A dedicated "Data" tab featuring a searchable, paginated table powered by `DT`.
* **Export Capabilities:** Export filtered datasets directly to **CSV, Excel, or PDF** for offline reporting.

---

## 🛠️ Tech Stack

* **Language:** R
* **Framework:** [Shiny](https://shiny.posit.co/)
* **UI/UX:** `shinydashboard`, `shinycssloaders` (for smooth transitions)
* **Visualization:** `plotly` (interactive plots), `RColorBrewer`
* **Data Manipulation:** `dplyr`, `readr`, `tidyr`
* **Tables:** `DT` (DataTables)

---

## 📂 Project Structure

```text
BMW_Dashboard/
├── app.R              # Main application logic (UI & Server)
├── data/
│   └── BMW sales data (2010-2024).csv
└── www/
    └── bmw_logo.png   # Dashboard assets

```

---

## ⚙️ Installation & Setup

1. **Clone the Repository:**
```bash
git clone https://github.com/yourusername/BMW-Sales-Dashboard.git

```


2. **Install Required R Packages:**
Open R or RStudio and run:
```r
install.packages(c("shiny", "shinydashboard", "dplyr", "plotly", 
                   "DT", "readr", "shinycssloaders", "RColorBrewer"))

```


3. **Run the App:**
```r
shiny::runApp()

```



---

## 📊 Data Cleaning Logic

The application includes a robust preprocessing layer that:

* Standardizes `Mileage`, `Price`, and `Sales_Volume` by stripping non-numeric characters.
* Handles missing values in critical categories like `Fuel_Type`.
* Ensures correct data typing for high-speed reactive filtering.

---

## 🤝 Contributing

Contributions are welcome! If you'd like to improve the visualizations or add new predictive features:

1. Fork the Project.
2. Create your Feature Branch (`git checkout -b feature/NewAnalysis`).
3. Commit your Changes (`git commit -m 'Add some NewAnalysis'`).
4. Push to the Branch (`git push origin feature/NewAnalysis`).
5. Open a Pull Request.

---

**Would you like me to generate a specific `Requirements.txt` or a `manifest.json` file for deploying this to ShinyApps.io?**
