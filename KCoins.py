from manim_slides import Slide, ThreeDSlide
import random
from manim import *
from GraphManim import *
from scipy.stats import multivariate_normal
from icons import *

from GraphManim import *

class AnimListStartError(Exception):
    def __init__(self, message):            
        # Call the base class constructor with the parameters it needs
        super().__init__(self, message)

class Pocket(VGroup):
    def __init__(self, **kwargs):
        position_list = [
            [-1.5, 1, 0],  # top left
            [-1.5, -1.2, 0],  # bottom left
            [0, -1.8, 0],  #  bottom
            [1.5, -1.2, 0],  # bottom right
            [1.5, 1, 0],  # top right
        ]
        VGroup.__init__(self, **kwargs)
        Outline = Polygon(*position_list, color=WHITE).scale(.8)
        Inner = Outline.copy().scale(.9)
        self.add(Outline)
        self.add(DashedVMobject(Inner, num_dashes=40))

class AnimationList:
    def __init__(self, times = None, animations = None, re_orderings = None, removes = None, adds = None):
        if times == None:
            times = []
        if animations == None:
            animations = []
        if re_orderings == None:
            re_orderings = []
        if removes == None:
            removes = []
        if adds == None:
            adds = []
        self.times = times
        self.animations = animations
        self.re_orderings = re_orderings
        self.adds = adds
        self.removes = removes

    def __str__(self):
        result = "\n".join([str(t) + "\t" + str(len(a)) for t,a  in zip(self.times, self.animations)])
        return result

    def length(self):
        return len(self.times)

    # Adds a new time step
    def time_step(self, time=.2):
        self.times.append(time)
        self.animations.append([])
        self.re_orderings.append([])
        self.removes.append([])
        self.adds.append([])

    def re_order(self, manim_objects):
        self.re_orderings[-1] = manim_objects

    def add_objects(self, manim_objects):
        self.adds[-1] += manim_objects

    def remove_objects(self, manim_objects):
        self.removes[-1] += manim_objects

    # Adds a new animation simultaneiously
    def add_animation(self, anim):
        if len(self.animations) == 0:
            raise AnimListStartError("Need to define time step first")
        self.animations[-1].append(anim)
    
    # Adds other after self
    def append(self, other):
        new_times = self.times + other.times
        new_anims = self.animations + other.animations
        new_adds = self.adds + other.adds
        new_removes = self.removes + other.removes
        new_reords = self.re_orderings + other.re_orderings
        return AnimationList(animations = new_anims, times = new_times, adds = new_adds, removes = new_removes, re_orderings=new_reords)
    
    # Adds other after self
    def simul(self, other):
        new_times = []
        new_anims = []
        new_reords = []
        new_adds = []
        new_removes = []
        min_length = min(len(self.times),len(other.times))
        for i in range(min_length):
            new_times.append(max(self.times[i], other.times[i]))
            new_anims.append(self.animations[i] + other.animations[i])
            new_reords.append(self.re_orderings[i] + other.re_orderings[i])
            new_adds.append(self.adds[i] + other.adds[i])
            new_removes.append(self.removes[i] + other.removes[i])
        
        if len(self.times) <= len(other.times):
            for time in other.times[min_length:] :
                new_times.append(time)
            for anim in other.animations[min_length:]:
                new_anims.append(anim)
        else:
            for time in self.times[min_length:] :
                new_times.append(time)
            for anim in self.animations[min_length:]:
                new_anims.append(anim)
        return AnimationList(animations = new_anims, times = new_times, adds = new_adds, removes = new_removes, re_orderings=new_reords)

    def play_animation(self, scene):
        for anims, time, reord, ads, rem in zip(self.animations, self.times, self.re_orderings, self.adds, self.removes):
            # Add
            for moj in ads:
                scene.add(moj)

            # Reorder
            for i, moj in enumerate(reord):
                moj.z_index = i

            # Animate
            scene.play(*anims, run_time = time)

            # Remove
            for moj in rem:
                scene.remove(moj)

class Coin(VGroup):
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
        self.original = self.copy()
        self.flip_outcomes = []

    def randomize_bias(self):
        new_bias = random.random()
        heads_angle = new_bias * 2 * PI
        #self.target = Coin(bias=new_bias).move_to(self.get_center())
        self.red_slice.target = AnnularSector(inner_radius=0, outer_radius=.3, angle=heads_angle, start_angle= PI / 2 - heads_angle/2, color=RED)
        self.blue_slice.target = AnnularSector(inner_radius=0, outer_radius=.3, angle=2 * PI - heads_angle, start_angle= PI / 2 + heads_angle/2, color=BLUE)
        self.red_slice.target.shift(self.get_center())
        self.blue_slice.target.shift(self.get_center())
        return [MoveToTarget(self.red_slice), MoveToTarget(self.blue_slice)]

    def reset_original(self):
        self.original = self.copy()

    def select(self, pocket):
        anim_list = AnimationList()
        anim_list.time_step(.5)
        self.generate_target()
        self.target.move_to(pocket.get_center() + 1.7 * UP)
        anim_list.add_animation(MoveToTarget(self))
        return anim_list
    
    def deselect(self):
        anim_list = AnimationList()
        anim_list.time_step(.5)
        self.target = self.original.copy()
        anim_list.add_animation(MoveToTarget(self))
        for flip_outcome in self.flip_outcomes:
            anim_list.add_animation(FadeOut(flip_outcome, shift = UP))
        self.flip_outcomes = []
        return anim_list

    # Create H and T sides
    def pre_flip(self):
        heads = VGroup()
        heads.add(Circle(radius=self.width/2, color=RED, fill_opacity=1))
        #heads.add(AnnularSector(inner_radius=0, outer_radius=self.width/2, angle=2 * PI, start_angle= PI / 2, color=RED))
        heads.add(Text("H", font_size=22, color=BLACK).move_to(heads.get_center()))
        heads.move_to(self.get_center())

        tails = VGroup()
        tails.add(Circle(radius=self.width/2, color=BLUE, fill_opacity=1))
        #tails.add(AnnularSector(inner_radius=0, outer_radius=self.width/2, angle=2 * PI, start_angle= 3 *PI / 2, color=BLUE))
        tails.add(Text("T", font_size=22, color=BLACK).move_to(tails.get_center()))
        tails.move_to(self.get_center())
        return heads, tails


    def flip(self, final='H',n_flips=1):
        heads, tails = self.pre_flip()
        anim_list = AnimationList()

        if final=="H":
            HT = [tails, heads]
        else:
            HT = [heads, tails]

        def collapse(c):
            c.generate_target()
            c.target.stretch(.01, dim=1)
            anim_list.add_animation(MoveToTarget(c))

        def relapse(c):
            c.generate_target()
            c.target.stretch(1, dim=1)
            anim_list.add_animation(MoveToTarget(c))

        # Collapse All

        anim_list.time_step(.2)
        anim_list.add_objects([heads, tails])
        collapse(self)
        collapse(tails)
        collapse(heads)

        for i in range(n_flips * 2):
            # Relapse HT[0], and collapse again
            anim_list.time_step(.2)
            index_mod = i % 2
            anim_list.re_order([self, HT[index_mod - 1], HT[index_mod]])
            relapse(HT[index_mod])
            anim_list.time_step(.2)
            if i < n_flips * 2 - 1:
                collapse(HT[index_mod])
        self.flip_outcomes += [HT[index_mod]]
        anim_list.remove_objects([HT[index_mod - 1]])
        relapse(self)
        return anim_list
    
    def second_flip_split(self, n_flips=1):
        anim_list = AnimationList()

        anim_list.time_step(.5)
        self.generate_target()
        self.target.shift(.7 * RIGHT)
        anim_list.add_animation(MoveToTarget(self))

        for f_o in self.flip_outcomes:
            f_o.generate_target()
            f_o.target.shift(.7 * LEFT)
            anim_list.add_animation(MoveToTarget(f_o))
        return anim_list

    def get_outcome_components(self, end_y_coord):
        outcome_text = self.flip_outcomes[0][1]
        outcome_background = self.flip_outcomes[0][0]
        self.flip_outcomes = []
        outcome_text.generate_target()
        if outcome_text.text == "H":
            outcome_text.target.color = RED
        else:
            outcome_text.target.color = BLUE
        end_location = outcome_text.get_center()
        end_location[1] = end_y_coord
        outcome_text.target.move_to(end_location)
        return outcome_text, FadeOut(outcome_background)

class KCoins(Slide):

    def change_title(self, new_title, old, everything):
        new = Text(new_title).to_edge(UP, buff=.5)
        self.play(TransformMatchingShapes(old, new), FadeOut(everything))
        return new

    def construct(self):
        title = Text("The Power of Higher-Order Moments").to_edge(UP, buff=.5)
        self.add(title)

        # Pockets
        Left_Pocket = Pocket().shift(3 * LEFT + 2.2 * DOWN)
        Right_Pocket = Pocket().shift(3 * RIGHT + 2.2 * DOWN)

        # Coins
        H_biased = Coin(bias=3/4)
        T_biased = Coin(bias=1/4)
        T_biased.move_to(H_biased.get_center() + DOWN)
        Biased_Coins = VGroup(H_biased, T_biased)
        Biased_Coins.move_to(Left_Pocket.get_center())

        Fair1 = Coin()
        Fair2 = Coin()
        Fair2.move_to(Fair1.get_center() + DOWN)
        Fair_Coins = VGroup(Fair1, Fair2)
        Fair_Coins.move_to(Right_Pocket.get_center())

        # Remember current location
        for coin in [H_biased, T_biased, Fair1, Fair2]:
            coin.reset_original()
        Coins = VGroup(Biased_Coins, Fair_Coins)

        # Coin Labels
        #H_biased_label = Text("Coin #1", font_size = 18)
        #H_biased_label.next_to(H_biased, DOWN)
        #T_biased_label = Text("Coin #2", font_size = 18)
        #T_biased_label.next_to(T_biased, DOWN)
        #Fair1_label = Text("Coin #1", font_size = 18)
        #Fair1_label.next_to(Fair1, DOWN)
        #Fair2_label = Text("Coin #2", font_size = 18)
        #Fair2_label.next_to(Fair2, DOWN)
        #Coin_Labels = VGroup(H_biased_label, T_biased_label, Fair1_label, Fair2_label)

        # Bar charts
        def get_bar_percentages(trackers):
            total = 0
            for tracker in trackers:
                total += tracker.get_value()
            if total > 0:
                return [tracker.get_value()/total for tracker in trackers]
            else:
                return [0 for tracker in trackers]
            
        left_trackers = {"H": ValueTracker(0), 
                         "T": ValueTracker(0), 
                         "HH": ValueTracker(0), 
                         "HT": ValueTracker(0), 
                         "TH": ValueTracker(0), 
                         "TT": ValueTracker(0)}
        BC_Left = BarChart(
            values=[0, 0],
            bar_names=["H", "T"],
            y_range=[0, 1, .5],
            y_length=2,
            x_length=3,
            x_axis_config={"font_size": 24},
            bar_colors=[RED, BLUE]
        )
        BC_Left.scale(.8).shift(3 * LEFT + 1.5 * UP)
        BC_Left.add_updater(
            lambda mob: mob.change_bar_values(get_bar_percentages([left_trackers["H"], left_trackers["T"]])))
        
        BC_Left2 = BarChart(
            values=[0, 0, 0, 0],
            bar_names=["HH", "HT", "TH", "TT"],
            y_range=[0, 1, .5],
            y_length=2,
            x_length=3,
            x_axis_config={"font_size": 24},
            bar_colors=[RED, PURPLE, PURPLE, BLUE]
        )
        BC_Left2.scale(.8).shift(3 * LEFT + 1.5 * UP)
        BC_Left2.add_updater(
            lambda mob: mob.change_bar_values(get_bar_percentages([left_trackers[ht] for ht in BC_Left2.bar_names])))

        right_trackers = {"H": ValueTracker(0), 
                         "T": ValueTracker(0), 
                         "HH": ValueTracker(0), 
                         "HT": ValueTracker(0), 
                         "TH": ValueTracker(0), 
                         "TT": ValueTracker(0)}
        BC_Right = BarChart(
            values=[0, 0],
            bar_names=["H", "T"],
            y_range=[0, 1, .5],
            y_length=2,
            x_length=3,
            x_axis_config={"font_size": 24},
            bar_colors=[RED, BLUE]
        )
        BC_Right.scale(.8).shift(3 * RIGHT + 1.5 * UP)
        BC_Right.add_updater(
            lambda mob: mob.change_bar_values(get_bar_percentages([right_trackers["H"], right_trackers["T"]]))
        )

        BC_Right2 = BarChart(
            values=[0, 0, 0, 0],
            bar_names=["HH", "HT", "TH", "TT"],
            y_range=[0, 1, .5],
            y_length=2,
            x_length=3,
            x_axis_config={"font_size": 24},
            bar_colors=[RED, PURPLE, PURPLE, BLUE]
        )
        BC_Right2.scale(.8).shift(3 * RIGHT + 1.5 * UP)
        BC_Right2.add_updater(
            lambda mob: mob.change_bar_values(get_bar_percentages([right_trackers[ht] for ht in BC_Right2.bar_names]))
        )


        # ~~~~~~~~~~~~~~~ANIMATION~~~~~~~~~~~~~~~~~~~
        # Subroutines
        def flip_step(left_coin, right_coin, left_final, right_final):
            left_coin.select(Left_Pocket).simul(right_coin.select(Right_Pocket)).play_animation(self)
            left_coin.flip(final=left_final).simul(right_coin.flip(final=right_final)).play_animation(self)
            #self.play(left_trackers[left_final].animate.set_value(left_trackers[left_final].get_value() + 1), 
            #          right_trackers[right_final].animate.set_value(right_trackers[right_final].get_value() + 1))
            deselecting = left_coin.deselect().simul(right_coin.deselect())
            deselecting.add_animation(left_trackers[left_final].animate.set_value(left_trackers[left_final].get_value() + 1))
            deselecting.add_animation(right_trackers[right_final].animate.set_value(right_trackers[right_final].get_value() + 1))
            deselecting.play_animation(self)
            self.wait(.2)
            self.next_slide()

        def two_flip_step(left_coin, right_coin, left_finals, right_finals):
            left_coin.select(Left_Pocket).simul(right_coin.select(Right_Pocket)).play_animation(self)
            left_coin.flip(final=left_finals[0]).simul(right_coin.flip(final=right_finals[0])).play_animation(self)
            left_coin.second_flip_split().simul(right_coin.second_flip_split()).play_animation(self)
            left_coin.flip(final=left_finals[1]).simul(right_coin.flip(final=right_finals[1])).play_animation(self)
            #self.play(left_trackers[left_final].animate.set_value(left_trackers[left_final].get_value() + 1), 
            #          right_trackers[right_final].animate.set_value(right_trackers[right_final].get_value() + 1))
            deselecting = left_coin.deselect().simul(right_coin.deselect())
            deselecting.add_animation(left_trackers[left_finals].animate.set_value(left_trackers[left_finals].get_value() + 1))
            deselecting.add_animation(right_trackers[right_finals].animate.set_value(right_trackers[right_finals].get_value() + 1))
            deselecting.play_animation(self)
            self.wait(.2)
        self.next_slide()
        # Title

        # Create
        self.play(FadeIn(Coins, shift=DOWN), Create(Left_Pocket), Create(Right_Pocket), run_time=1)
        self.wait(.2)
        self.next_slide()
        self.play(Create(BC_Left), Create(BC_Right), run_time = .5)
        self.wait(.2)
        self.next_slide()

        # Flip Single Coins
        flip_step(H_biased, Fair2, "H", "T")
        flip_step(T_biased, Fair2, "T", "H")
        flip_step(T_biased, Fair1, "T", "H")

        # Distribution that we converge to
        self.play(left_trackers["H"].animate.set_value(1),
                  left_trackers["T"].animate.set_value(1),
                  right_trackers["H"].animate.set_value(1),
                  right_trackers["T"].animate.set_value(1), run_time = 1)
        self.wait(.2)
        self.next_slide()

        # Flip Two Coins
        # Morph the charts
        self.play(FadeOut(BC_Left), FadeOut(BC_Right), run_time = .5)
        self.play(Create(BC_Left2), Create(BC_Right2), run_time = .5)
        self.wait(.2)
        self.next_slide()
        
        two_flip_step(H_biased, Fair1, "HH", "TH")
        two_flip_step(T_biased, Fair2, "TT", "HH")
        two_flip_step(H_biased, Fair2, "HH", "HT")
        two_flip_step(T_biased, Fair1, "TH", "TT")

        # Distribution that we converge to
        self.play(left_trackers["HH"].animate.set_value(5),
                  left_trackers["HT"].animate.set_value(3),
                  left_trackers["TH"].animate.set_value(3),
                  left_trackers["TT"].animate.set_value(5), run_time = 1)
        self.wait(.2)
        self.next_slide()

        everything = VGroup(BC_Right2, BC_Left2, Left_Pocket, Right_Pocket, Biased_Coins, Fair_Coins)
        self.change_title("The k-Coin Problem", title, everything=everything)

        X1 = Vertex(label = r'R')
        X2 = Vertex(label = r'R')
        X3 = Vertex(label = r'R')
        X = VGroup(X1, X2, X3)
        VisibleSubvertices = VGroup(X1, X2, X3).arrange(RIGHT, buff=1.5)
        U = Vertex(label=r'U', observed=False).next_to(VisibleSubvertices, 2.5 * UP)

        FullGraph = VGroup(VisibleSubvertices, U)
        FullGraph.next_to(title, 3 * DOWN)
        U_Edges = VGroup(*[Edge(U, V, observed = False) for V in VisibleSubvertices])
        self.add(FullGraph, U_Edges)
        self.wait(.2)
        self.next_slide()

        # Vectorize
        U_vec = MobjectMatrix([[MathTex(r"\mathbb{P}(u^{(0)})").scale(.3), MathTex(r"\mathbb{P}(u^{(1)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, h_buff=0.7).next_to(U, UP)
        P_vec = lambda ob: MobjectMatrix([[MathTex(r"\mathbb{E}(" + ob.get_label_text() + r"|u^{(0)})").scale(.3)], [MathTex(r"\mathbb{E}(" + ob.get_label_text() + r"|u^{(1)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5).next_to(ob, DOWN)
        def E_vec(lb, cond="", exp=""):
            return MobjectMatrix([[MathTex(r"\mathbb{E}(" + lb + r"|" + cond + r" u^{(0)})" + exp).scale(.3)], [MathTex(r"\mathbb{E}(" + lb + r"|" + cond + r" u^{(1)})" + exp).scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5)
        
        col_vecs = VGroup(*[P_vec(v) for v in VisibleSubvertices])
        self.play(Write(U_vec), Write(col_vecs))
        self.wait(.2)
        self.next_slide()

        # Observed Moments
        first_order_eq = VGroup(
            CoinMatrix(data = ["H"], reveal_boundary = (1, 1)).matrix_of_coins,
            MathTex(r"\mathbb{E}(R) = "),
            U_vec.copy(),
            E_vec("Y")
        ).arrange(RIGHT)
        second_order_eq = VGroup(
            CoinMatrix(data = ["HH"], reveal_boundary = (2, 1)).matrix_of_coins,
            MathTex(r"\mathbb{E}(R^{\odot 2}) = "),
            U_vec.copy(),
            E_vec("R", exp = r"^{2}")
        ).arrange(RIGHT)
        third_order_eq = VGroup(
            CoinMatrix(data = ["HHH"], reveal_boundary = (3, 1)).matrix_of_coins,
            MathTex(r"\mathbb{E}(R^{\odot 3}) = "),
            U_vec.copy(),
            E_vec("R", exp = r"^{3}")
        ).arrange(RIGHT)
        eqs = VGroup(first_order_eq, second_order_eq, third_order_eq).arrange(DOWN, center=True).to_corner(DOWN, buff = .5)
        for eq in eqs:
            self.play(Write(eq))
            self.wait(.2)
            self.next_slide()

        everything = VGroup(eqs, U_vec, col_vecs, FullGraph)
        everything.generate_target()
        everything.target.to_edge(LEFT, buff = .5)
        self.play(MoveToTarget(everything))
        self.wait(.2)
        self.next_slide()

        dof1 = Tex(r"Degrees of Freedom:").scale(.7)
        dof2 = Tex(r"$k-1$ from $U$").scale(.5)
        dof3 = Tex(r"$k$ from the conditionals for $R$").scale(.5)
        identification = Tex(r"Identifiability with $2k-1$ moments", color=GREEN).scale(.7)
        citation = Paragraph("J. Li, Rabani, L. Schulman, C. Swamy (2015).\nLearning Arbitrary Statistical Mixtures of Discrete Distributions.\nIn Symposium on Theory of Computing.").scale(.25)
        ident_result = VGroup(dof1, dof2, dof3, identification, citation).shift(.5 * DOWN)
        ident_result.arrange(DOWN, center=True).to_edge(RIGHT, buff = .5)
        for t in ident_result:
            self.play(Write(t))
            self.wait(.2)
            self.next_slide()




