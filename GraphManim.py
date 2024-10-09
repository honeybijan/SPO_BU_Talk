from manim import *

class Graph(VGroup):
    def __init__(self, V, E, unobserved = None, **kwargs):
        VGroup.__init__(self, **kwargs)
        self.Vertices = VGroup(*V)
        self.Edges = VGroup(*E)
        self.add(self.Vertices, self.Edges)
        if unobserved:
            self.unobserved = unobserved
            self.add(unobserved)

    def get_edges(self):
        return self[1]
    
    def get_vertices(self):
        return self[0]

class Vertex(VGroup):
    def __init__(self, label, observed=True, color = WHITE, radius = .2, **kwargs):
        VGroup.__init__(self, **kwargs)
        self.label_text = label
        self.Outer_Circle = Circle(radius = radius, color=color)
        if not observed:
            self.Outer_Circle = DashedVMobject(self.Outer_Circle, num_dashes = 20)
        self.add(self.Outer_Circle)
        self.Label = MathTex(label, font_size = 18, color=color)
        self.Label.move_to(self.get_center())
        self.Label.add_updater(lambda mob: mob.become(MathTex(self.get_label_text(), font_size=18).move_to(mob.get_center()))) #Then try get_label_text -> mob.text, and changing that in reset text
        self.add(self.Label)
        Cond_Circle = Circle(radius = radius, color = color, fill_opacity=1)
        Cond_Label = MathTex(label, font_size = 18, color=BLACK)
        self.DarkMode = VGroup(Cond_Circle, Cond_Label)

    def get_label_text(self):
        return self.label_text

    def reset_text(self, label):
        self.label_text = label
    
    def highlight(self, color):
        return [self.Label.animate.set_color(color), self.Outer_Circle.animate.set_color(color)]
    
    def unhighlight(self):
        return self.highlight(WHITE)
    
    def condition(self):
        self.DarkMode.move_to(self.get_center())
        return FadeIn(self.DarkMode)
    
    def uncondition(self):
        return FadeOut(self.DarkMode)
    
class RectVertex(VGroup):
    def __init__(self, label, observed=True, **kwargs):
        VGroup.__init__(self, **kwargs)
        self.Label = Tex(label, font_size = 18, color=WHITE)
        self.Outer_Rect = SurroundingRectangle(self.Label, color=WHITE, buff = .1)
        if not observed:
            Outer_Rect = DashedVMobject(Outer_Rect, num_dashes = 30)
        self.add(self.Label)
        self.add(self.Outer_Rect)
    
    def highlight(self, color):
        return self.set_color(color) #[self.Label.animate.set_color(color), self.Outer_Circle.animate.set_color(color)]
    
    def unhighlight(self):
        return self.highlight(WHITE)
    
    def condition(self):
        self.Cond_Rect = self.Outer_Rect.copy()
        self.Cond_Rect.set_fill(color = self.Cond_Rect.get_color(), opacity = 1)
        self.Cond_Label = self.Label.copy()
        self.Cond_Label.set_color(color=BLACK)
        return [FadeIn(self.Cond_Rect), FadeIn(self.Cond_Label)]
    
    def uncondition(self):
        return [FadeOut(self.Cond_Rect), FadeOut(self.Cond_Label)]
    
def dist_sq(p1, p2):
    return (p1[0] - p2[0])**2 + (p1[1] - p2[1])**2

class RectEdge(VGroup):
    def __init__(self, Vertex1, Vertex2, observed=True, label=None, label_loc = UP, curve=None, **kwargs):
        VGroup.__init__(self, **kwargs)
        dist_points = [(dist_sq(Vertex1.get_center(), p2), p2) for p2 in [Vertex2.get_edge(UP), Vertex2.get_edge(DOWN), Vertex2.get_edge(LEFT), Vertex2.get_edge(RIGHT)]]
        end_point = min(dist_points)[1]
        dist_points = [(dist_sq(Vertex2.get_center(), p2), p2) for p2 in [Vertex1.get_edge(UP), Vertex1.get_edge(DOWN), Vertex1.get_edge(LEFT), Vertex1.get_edge(RIGHT)]]
        start_point = min(dist_points)[1]
        if not curve:
            if observed:
                Edge_Arrow = Line(start=start_point, end=end_point, buff=0)
            else:
                Edge_Arrow = DashedLine(start=start_point, end=end_point, buff=0)
        else:
            Edge_Arrow = ArcBetweenPoints(start=start_point, end=end_point, buff=0, angle=curve)
        Edge_Arrow.add_tip(tip_width = .1, tip_length=.05)
        self.add(Edge_Arrow)
        if label:
            self.Label = Text(label, font_size = 18, color=WHITE).next_to(Edge_Arrow, label_loc)
            self.Label.next_to(Edge_Arrow, label_loc)
            self.add(self.Label)
    
    def highlight(self, color):
        return [self.Label.animate.set_color(color), self.Outer_Circle.animate.set_color(color)]
    
    def unhighlight(self):
        return self.highlight(WHITE)
    
    def condition(self):
        return [self.Outer_Circle.animate.set_fill_opacity(1), self.Label.animate.set_color(BLACK)]
    
    def uncondition(self):
        return [self.Outer_Circle.animate.set_fill_opacity(0), self.Label.animate.set_color(WHITE)]
    
class Edge(VGroup):
    def __init__(self, Vertex1, Vertex2, observed=True, redraw = True, label=None, label_loc = UP, curve=None, angle = None, tip_size = .1, **kwargs):
        VGroup.__init__(self, **kwargs)
        if not curve:
            if redraw:
                if observed:
                    Edge_Arrow = always_redraw(lambda: Line(start=Vertex1, end=Vertex2, buff=0).add_tip(tip_width = tip_size, tip_length = tip_size))
                else:
                    Edge_Arrow = always_redraw(lambda: DashedLine(start=Vertex1, end=Vertex2, buff=0).add_tip(tip_width = tip_size, tip_length = tip_size))
            else:
                if observed:
                    Edge_Arrow = Line(start=Vertex1, end=Vertex2, buff=0).add_tip(tip_width = tip_size, tip_length = tip_size)
                else:
                    Edge_Arrow = DashedLine(start=Vertex1, end=Vertex2, buff=0).add_tip(tip_width = tip_size, tip_length = tip_size)
        else:
            radius = Vertex1.Outer_Circle.radius
            if Vertex1.get_center()[0] < Vertex2.get_center()[0]:
                s = Vertex1.get_center() + np.sin(angle) * radius * UP + np.cos(angle) * radius * RIGHT
                e = Vertex2.get_center() + np.sin(angle) * radius * UP + np.cos(angle) * radius * LEFT
            else:
                s = Vertex1.get_center() + np.sin(angle) * radius * UP + np.cos(angle) * radius * LEFT
                e = Vertex2.get_center() + np.sin(angle) * radius * UP + np.cos(angle) * radius * RIGHT
            Edge_Arrow = ArcBetweenPoints(start=s, end=e, angle=curve).add_tip(tip_width = tip_size, tip_length = tip_size)
        self.add(Edge_Arrow)
        if label:
            self.Label = MathTex(label, font_size = 18, color=WHITE).next_to(Edge_Arrow, label_loc)
            self.add(self.Label)
    