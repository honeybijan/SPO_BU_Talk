from manim_slides import Slide, ThreeDSlide
import random
from manim import *
from GraphManim import *
from scipy.stats import multivariate_normal
from icons import *

from GraphManim import *

class Conclusion(Slide):

    def change_title(self, new_title, old, everything):
        new = Text(new_title).to_edge(UP, buff=.5)
        self.play(TransformMatchingShapes(old, new), FadeOut(everything))
        return new
    

    def construct(self):
        title = Text("Hierarchy of Identifiability").to_edge(UP, buff=.5)
        self.add(title)
        self.wait(.2)
        self.next_slide()

        Z, T, Y, X = Vertex("Z"), Vertex("T"), Vertex("Y"), Vertex("X")
        Vertices = VGroup(Z, T, Y, X).arrange(RIGHT, buff = .5)
        U = Vertex("U", observed=False).next_to(Vertices, UP, buff=.3)
        Edges = VGroup(Edge(T,Y), Edge(Z,T), Edge(X,Y))
        U_Edges = VGroup(*[Edge(U, V, observed=False) for V in Vertices]) 
        ATE_DAG = VGroup(Vertices, U, Edges, U_Edges)
        
        Z, T, Y, X = Vertex("Z"), Vertex("T"), Vertex("Y"), Vertex("X")
        Vertices = VGroup(Z, T, Y, X).arrange(RIGHT, buff = .5)
        U = Vertex("U", observed=False).next_to(Vertices, UP, buff=.3)
        Edges = VGroup(Edge(T,Y), Edge(Z,T))
        U_Edges = VGroup(*[Edge(U, V, observed=False) for V in Vertices]) 
        CATE_DAG = VGroup(Vertices, U, Edges, U_Edges)

        Z, T, Y, X = Vertex("Z"), Vertex("T"), Vertex("Y"), Vertex("X")
        Vertices = VGroup(Z, T, Y, X).arrange(RIGHT, buff = .5)
        U = Vertex("U", observed=False).next_to(Vertices, UP, buff=.3)
        Edges = VGroup(Edge(T,Y))
        U_Edges = VGroup(*[Edge(U, V, observed=False) for V in Vertices]) 
        MIX_DAG = VGroup(Vertices, U, Edges, U_Edges)
        
        Z, T, Y, X = Vertex("Z"), Vertex("T"), Vertex("Y"), Vertex("X")
        Vertices = VGroup(T, Y).arrange(RIGHT, buff = .5)
        obs_U = Vertex("U").move_to(U)
        obs_U_Edges = VGroup(*[Edge(obs_U, V) for V in Vertices])
        Edges = VGroup(Edge(Y,T))
        OBS_DAG = VGroup(Vertices, obs_U, obs_U_Edges, Edges)

        tiers = []
        rects = []
        for dag, component in zip([OBS_DAG, MIX_DAG, CATE_DAG, ATE_DAG], ["HTE", "Mixture", "MTE", "ATE"]):
            label = Text(component).scale(.5)
            label_dag = VGroup(label, dag.scale(.75)).arrange(DOWN, buff = .1)
            tiers.append(label_dag)

        stack = VGroup(*tiers).arrange(DOWN, buff = .2).next_to(title, DOWN, buff = 1)
        running_stack = []
        for item in stack:
            running_stack.append(item)
            rect = SurroundingRectangle(VGroup(*running_stack), color=WHITE, corner_radius = .1)
            running_stack.append(rect)
            self.play(Create(item), Create(rect))
            self.wait(.2)
        self.next_slide()
        full_stack = VGroup(*running_stack)

        full_stack.generate_target()
        full_stack.target.to_edge(LEFT, buff = .5)
        self.play(MoveToTarget(full_stack))
        self.wait(.2)
        self.next_slide()

        conc1 = Text("1. Clustering-like methods (e.g. IPW and covariate adjustments) are fundimetally limited.")
        conc2 = Text("2. In the absence of validation, we need to think about identifiability and sample complexity.")
        conc3 = Text("3. When developing methods, we should recover solutions at the granularity of interest.")
        
        conclusions = VGroup(conc1.scale(.38), conc2.scale(.38), conc3.scale(.38)).arrange(DOWN, aligned_edge = LEFT, buff = .5).to_edge(RIGHT, buff = .5)
        for conc in conclusions:
            self.wait(.2)
            self.next_slide()
            self.play(Create(conc))

        citation = Paragraph("B. Mazaheri, C. Squires and C. Uhler (2024).\nSynthetic Potential Outcomes and Causal Mixture Identifiability.\nPreprint on arXiv, new version soon!").scale(.4)
        citation.to_corner(DR, buff=.5)
        self.wait(.2)
        self.next_slide()
        self.play(Create(citation))
        self.wait(.2)
        self.next_slide()

        everything = VGroup(conclusions, full_stack, citation)
        title = self.change_title("Thanks", title, everything=everything)

        colab = Text("Current Collaborators").scale(.5)
        col1 = Text("Chandler Squires (Postdoc at Carnegie Mellon)").scale(.3)
        col2 = Text("Caroline Uhler (Director of Schmidt Center, Professor at MIT)").scale(.3)
        colabs = VGroup(colab, col1, col2).arrange(DOWN)

        pcolab = Text("PhD Collaborators").scale(.5)
        pcol1 = Text("Leonard Schulman (Professor at Caltech)").scale(.3)
        pcol2 = Text("Shuki Bruck (Professor at Caltech)").scale(.3)
        pcol3 = Text("Yuval Rabani (Professor at Hebrew University of Jerusalem)").scale(.3)
        pcol4 = Text("Spencer Gordon (Research Software Engineer, Krypton Labs)").scale(.3)
        pcolabs = VGroup(pcolab, pcol1, pcol2, pcol3, pcol4).arrange(DOWN)

        funding = Text("Funding").scale(.5)
        fund1 = Text("The Eric and Wendy Schmidt Center").scale(.3)
        fund2 = Text("Broad Institute of MIT and Harvard").scale(.3)
        funds = VGroup(funding, fund1, fund2).arrange(DOWN)

        things = VGroup(colabs, pcolabs, funds).arrange(DOWN, buff = .5).next_to(title, DOWN, buff = .5)
        for thing in things:
            self.play(Create(thing))
            self.wait(.2)
            self.next_slide()

        dartmouth = Paragraph("I am recruiting students for my lab at Dartmouth!").scale(.5).next_to(things, DOWN, buff = .5)
        self.play(Create(dartmouth))
        self.wait(.2)







