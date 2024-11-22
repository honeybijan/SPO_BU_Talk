from manim import *
import random


class CoinProb(VGroup):
    def __init__(self, radius = .3, bias = .5, **kwargs):
        VGroup.__init__(self, **kwargs)
        self.bias = bias
        heads_angle = bias * 2 * PI
        self.red_slice = AnnularSector(inner_radius=0, outer_radius=radius, angle=heads_angle, start_angle= PI / 2 - heads_angle/2, color=RED)
        self.blue_slice = AnnularSector(inner_radius=0, outer_radius=radius, angle=2 * PI - heads_angle, start_angle= PI / 2 + heads_angle/2, color=BLUE)
        self.add(self.red_slice)
        self.add(self.blue_slice)
        H = Text("H", font_size = 14, color=BLACK)
        H.move_to(self.get_center() + .15 * UP)
        T = Text("T", font_size = 14, color=BLACK)
        T.move_to(self.get_center() + .15 * DOWN)
        self.add(H)
        self.add(T)

class FlippedCoin(VGroup):
    def __init__(self, radius = .2, side = "H", **kwargs):
        VGroup.__init__(self, **kwargs)
        if side == "H":
            col = RED
        else:
            col = BLUE
        self.coin = Dot(radius = radius, color = col)
        result = Text(side, font_size = 14, color=BLACK)
        result.move_to(self.get_center())
        self.add(self.coin)
        self.add(result)

class CoinMatrix(VGroup):
    # Should take in something like data = ["HHTH", "HHHH", "TTTT"], groups = [RED, RED, BLUE]
    def __init__(self, data, reveal_boundary):
        gap = .33
        self.certainty = 0
        self.matrix_of_coins = VGroup()
        for row, flip_string in enumerate(data):
            row_of_coins = VGroup()
            for col, flip in enumerate(flip_string):
                coin = FlippedCoin(side = flip, radius = .15).shift(gap * row * DOWN + gap * col * RIGHT)
                row_of_coins.add(coin)
            self.matrix_of_coins.add(row_of_coins)
        self.reveal_boundary_r, self.reveal_boundary_c = reveal_boundary

    def reveal_initial(self):
        fades = []
        for row in range(self.reveal_boundary_r):
            for col in range(self.reveal_boundary_c):
                fades += [FadeIn(self.matrix_of_coins[row][col])]
        return fades
    
    def reveal_flip(self, inc = .33):
        fades = []
        for row in range(self.reveal_boundary_r):
            fades += [FadeIn(self.matrix_of_coins[row][self.reveal_boundary_c])]
        self.reveal_boundary_c += 1
        return fades

    def reveal_sample(self):
        fades = []
        for col in range(self.reveal_boundary_c):
            fades += [FadeIn(self.matrix_of_coins[self.reveal_boundary_r][col])]
        self.reveal_boundary_r += 1
        return fades

class StickFigure(VGroup):
    def __init__(self, happy = True, color=WHITE, **kwargs):
        VGroup.__init__(self, **kwargs)
        self.color = color
        self.happy = happy
        self.body = SVGMobject(file_name = "svgs/person.svg", color = self.color, fill_color = color)
        self.add(self.body)
        self.eye1 = Circle(radius=.1, color=BLACK, fill_opacity=1).center().shift(.5 * LEFT)
        self.eye2 = Circle(radius=.1, color=BLACK, fill_opacity=1).center().shift(.5 * RIGHT)
        self.mouth = Arc(radius=1.0, start_angle= PI / 6, angle=2 * PI / 3, num_components=9, arc_center=self.body.get_center() + UP, color = BLACK, stroke_width=4).set_opacity(1).center().shift(DOWN)
        if self.happy:
            self.mouth.rotate(PI)
        self.head = VGroup(self.eye1, self.eye2, self.mouth).scale(.2).shift(1.3 * UP)
        self.add(self.head)

    def change_color(self, col):
        self.color = col
        self.body.set_color(color = col)
        self.head.set_color(color = BLACK)

    # p is prob of same, i.e. p is prob of happy if starting happy
    def fade_in_random_emotion(self, p):
        if (random.random() > p):
            self.mouth.rotate(PI)
            self.happy = not self.happy
        return FadeIn(self.head)
    
    def random_emotion(self, p):
        if (random.random() > p):
            self.mouth.rotate(PI)
            self.happy = not self.happy

    def group_of_sticks(n, width, scale, h_sep, v_sep, happy_prob=.5, red_prob = None):
        g = VGroup()
        if red_prob:
            red_inds = random.sample(list(range(n)), int(n * red_prob))
        for i in range(n):
            if not red_prob:
                col = random_bright_color()
            else:
                if i in red_inds:
                    col = RED
                else:
                    col = BLUE
            stick = StickFigure(color = col).scale(scale).shift(int(i / width) * v_sep * DOWN + int(i % width) * h_sep * RIGHT)
            if not red_prob:
                stick.random_emotion(happy_prob)
            else:
                if col == RED:
                    stick.random_emotion(.25)
                else:
                    stick.random_emotion(.75)
            g.add(stick)
        return g

class Pill(VGroup):
    def __init__(self, treated = True, color=WHITE, **kwargs):
        VGroup.__init__(self, **kwargs)
        body = SVGMobject(file_name = "svgs/pill.svg", color = color, fill_color = color).scale(.25)
        self.add(body)
        if not treated:
            prohibit = SVGMobject(file_name = "svgs/prohibit.svg", color = RED, fill_color = RED)
            self.add(prohibit)