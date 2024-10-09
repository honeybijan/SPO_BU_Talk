from manim_slides import Slide, ThreeDSlide
import random
from manim import *
from GraphManim import *
from scipy.stats import multivariate_normal
from icons import *

from GraphManim import *

class Intro(Slide):

    def change_title(self, new_title, old, everything):
        new = Text(new_title).to_edge(UP, buff=.5)
        self.play(TransformMatchingShapes(old, new), FadeOut(everything))
        return new
    

    def construct(self):
        title = VGroup(Text("Synthetic Potential Outcomes"), Text("and"), Text("Causal Mixture Identifiabilty")).arrange(DOWN).to_edge(UP, buff=1)
        self.add(title)

        name = Text("Bijan Mazaheri").scale(.75)
        position = Text("Postdoctoral Fellow").scale(.5)
        name_pos = VGroup(name, position).arrange(DOWN).next_to(title, DOWN, buff=.5)
        self.add(name_pos)

        schmidt = ImageMobject("schmidt.png").next_to(name_pos, DOWN, buff=.5)
        self.add(schmidt)
        self.wait()
        self.wait(run_time=.2)
        self.next_slide()








