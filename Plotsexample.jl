Set(1:3) ⊆ Set(0:5)
squares = [x^2 for x=1:10]

squares = []
for x in 1:10
push!(squares, x^2)
end
squares
println("Hello World!")

[x^2 for x in 0:9 if x > 5]

[(x, y) for x = 1:3 for y = 1:2]


using Plots
x = 1:10; y = rand(10); # These are the plotting data
plot(x, y)

x = 1:10; y = rand(10, 2) # 2 columns means two lines
plot(x, y)


x = 1:10; y = rand(10, 2) # 2 columns means two lines
plot(x, y)

z = rand(10)
plot!(x, z)

x = 1:10; y = rand(10, 2) # 2 columns means two lines
p = plot(x, y)
z = rand(10)
plot!(p, x, z)

p

x = 1:10; y = rand(10, 2) # 2 columns means two lines
plot(x, y, title = "Two Lines", label = ["Line 1" "Line 2"], lw = 3)

xlabel!("My x label")

gr() # Set the backend to GR
# This plots using GR
plot(x, y, title = "This is Plotted using GR")
plot!(x,z)

gr() # We will continue onward using the GR backend
plot(x, y, seriestype = :scatter, title = "My Scatter Plot")

scatter(x, y, title = "My Scatter Plot")

y = rand(10, 4)
plot(x, y, layout = (2,2))

plot(x,y)

p1 = plot(x, y) # Make a line plot
p2 = scatter(x, y) # Make a scatter plot
p3 = plot(x, y, xlabel = "This one is labelled", lw = 3, title = "Subtitle")
p4 = histogram(x, y) # Four histograms each with 10 points? Why not!
plot(p1, p2, p3, p4, layout = (2, 2), legend = false)


# Pkg.add("StatsPlots")
using StatsPlots # Required for the DataFrame user recipe
# Now let's create the DataFrame
using DataFrames
df = DataFrame(a = 1:10, b = 10 * rand(10), c = 10 * rand(10))
# Plot the DataFrame by declaring the points by the column names
@df df plot(:a, [:b :c]) # x = :a, y = [:b :c]. Notice this is two columns!

@df df scatter(:a, :b, title = "My DataFrame Scatter Plot!")

using Distributions
plot(Normal(3, 5), lw = 3)

y = rand(100, 4) # Four series of 100 points each
violin(["Series 1" "Series 2" "Series 3" "Series 4"], y, leg = false)
boxplot!(["Series 1" "Series 2" "Series 3" "Series 4"], y, leg = false)

using  RDatasets
iris = dataset("datasets", "iris")

using Plots
# 10 data points in 4 series
xs = range(0, 2π, length = 10)
data = [sin.(xs) cos.(xs) 2sin.(xs) 2cos.(xs)]

# We put labels in a row vector: applies to each series
labels = ["Apples" "Oranges" "Hats" "Shoes"]

# Marker shapes in a column vector: applies to data points
markershapes = [:circle, :star5]

# Marker colors in a matrix: applies to series and data points
markercolors = [
    :green :orange :black :purple
    :red   :yellow :brown :white
]

plot(
    xs,
    data,
    label = labels,
    shape = markershapes,
    color = markercolors,
    markersize = 10
)


using Plots
plot(
    range(0, 2π, length = 10),
    [sin.(xs) cos.(xs) 2sin.(xs) 2cos.(xs)],
    label = ["Apples" "Oranges" "Hats" "Shoes"],
    shape = [:circle, :star5],
    color =  [ :green :orange :black :purple  :red   :yellow :brown :white],
    markersize = 5
)

using StatsPlots, RDatasets
iris = dataset("datasets", "iris")
@df iris scatter(
    :SepalLength,
    :SepalWidth,
    group = :Species,
    m = (0.5, [:+ :h :star7], 12),
    bg = RGB(0.2, 0.2, 0.2)
)

using Plots
tmin = 0
tmax = 4π
tvec = range(tmin, tmax, length = 100)

plot(sin.(tvec), cos.(tvec))
plot(sin, cos, tvec)

plot(rand(100,4), layout = 4, label=["a" "b" "c" "d"], title=["1" "2" "3" "4"])

l = @layout [
    a{0.3w} [grid(1,3)
                  grid(1,3)
                  grid(1,3)
                  grid(1,2) ]
]

plot(
    rand(10, 12),
    layout = l, legend = false, seriestype = [:bar :scatter :path],
    title = ["($i)" for j in 1:1, i in 1:12], titleloc = :right, titlefont = font(8)
)
