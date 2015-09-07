open System.IO
open System.Windows.Forms
open System.Drawing

type Observation = { Label:int; Pixels: int[] }

let reader path = 
    let data = File.ReadAllLines path
    data.[1..]
    |> Array.map (fun line -> line.Split(','))
    |> Array.map (fun line -> line |> Array.map int)
    |> Array.map (fun line -> { Label = line.[0]; Pixels = line.[1..] })

let tileSize = 20

let draw (text:string) (pixels:int[]) =

    let form = new Form(TopMost = true, Visible = true, Width = 29 * tileSize, Height = 29 * tileSize)
                   
    let panel = new Panel(Dock = DockStyle.Fill)
    panel.BackColor <- Color.White
    form.Controls.Add(panel)

    let graphics = panel.CreateGraphics()
    let borders = new Pen(Color.Gray)
                
    pixels 
    |> Array.iteri (fun i p ->
        let col = i % 28
        let row = i / 28
        let color = Color.FromArgb(255 - int p, 255 - int p, 255 - int p)
        let brush = new SolidBrush(color)
        graphics.FillRectangle(brush,col*tileSize,row*tileSize,tileSize,tileSize)
        graphics.DrawRectangle(borders,col*tileSize,row*tileSize,tileSize,tileSize))

    let point = new PointF((float32)5, (float32)5)
    let font = new Font(family = FontFamily.GenericSansSerif, emSize = (float32)30)
    graphics.DrawString(text, font, new SolidBrush(Color.Red), point)
        
    form.Show()

let trainingPath = __SOURCE_DIRECTORY__ + @"../../Data/trainingsample.csv"
let data = reader trainingPath

let zero = data |> Seq.find (fun x -> x.Label = 0) 
let one  = data |> Seq.find (fun x -> x.Label = 1)
let anotherZero = data |> Seq.skip 100 |> Seq.find (fun x -> x.Label = 0)

draw "This is a Zero" zero.Pixels
draw "This is a One"  one.Pixels

let heatMap (obs1:Observation) (obs2:Observation) =
    (obs1.Pixels,obs2.Pixels) 
    ||> Array.map2 (fun x1 x2 -> abs (x1 - x2))
   
draw "Heatmap zero and other zero" (heatMap zero anotherZero)
draw "Heatmap one and other zero" (heatMap one anotherZero)