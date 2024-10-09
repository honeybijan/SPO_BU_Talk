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

class Everything(Slide):

    def change_title(self, new_title, old, everything):
        new = Text(new_title).to_edge(UP, buff=.5)
        self.play(TransformMatchingShapes(old, new), FadeOut(everything))
        return new
    
    def duplicate(self, node, old_arrow, U, new_labels):
        node_copy = node.copy()
        oa_copy = old_arrow.copy()
        new_left = Vertex(label=new_labels[0]).move_to(node).shift(LEFT)
        new_left_arrow = Edge(U, new_left, observed=False)
        new_right = Vertex(label=new_labels[1]).move_to(node).shift(RIGHT)
        new_right_arrow = Edge(U, new_right, observed=False)
        self.play(TransformMatchingShapes(node, new_left), 
                  TransformMatchingShapes(node_copy, new_right), 
                  TransformMatchingShapes(oa_copy, new_left_arrow), 
                  TransformMatchingShapes(old_arrow, new_right_arrow))
        return (new_left, new_right)
    

    def construct(self):
        title = VGroup(Text("Synthetic Potential Outcomes"), Text("and"), Text("Causal Mixture Identifiabilty")).arrange(DOWN).to_edge(UP, buff=1)
        self.add(title)

        name = Text("Bijan Mazaheri").scale(.75)
        position = Text("Postdoctoral Fellow").scale(.5)
        name_pos = VGroup(name, position).arrange(DOWN).next_to(title, DOWN, buff=.5)
        self.add(name_pos)

        schmidt = ImageMobject("schmidt.png").next_to(name_pos, DOWN, buff=.5)
        self.add(schmidt)
        self.wait(.2)
        self.next_slide()
        self.clear()

        title = Text("Mixture Models").to_edge(UP, buff=.5)
        self.add(title)

        latent_class = Tex(r"Latent class $U \in [k]$").scale(.5)
        class_specific_distribution = Tex(r"$k$ class-specific distributions $\mathbb{P}(\mathbf{V} \; | \; u)$").scale(.5)
        mixture_components = VGroup(latent_class, class_specific_distribution)
        mixture_components.arrange(DOWN, center=True).move_to(ORIGIN).shift(.5 * DOWN)

        V1 = Vertex(label = r'V_1')
        V2 = Vertex(label = r'V_2')
        V3 = Vertex(label = r'V_3')
        V4 = Vertex(label = r'V_4')
        VisibleSubvertices = VGroup(V1, V2, V3, V4).arrange(RIGHT, buff=1.5)
        U = Vertex(label = 'U', observed=False).next_to(VisibleSubvertices, UP, buff=1.5)

        U_Edges = VGroup(*[Edge(U, V, observed=False) for V in [V1, V2, V3, V4]])
        OtherEdges = VGroup(Edge(V1, V2), Edge(V2, V4, curve=1, angle = -1), Edge(V4, V3))

        latent_class.next_to(U, UP, buff = .3)
        class_specific_distribution.next_to(VGroup(VisibleSubvertices, OtherEdges), DOWN, buff = .3)

        FullGraph = VGroup(VisibleSubvertices, U, U_Edges, OtherEdges, latent_class, class_specific_distribution).next_to(title, DOWN, buff=1)

        self.play(Write(U), Write(latent_class), run_time = .5)
        self.wait(.2)
        self.next_slide()
        self.play(Write(VisibleSubvertices), Write(OtherEdges), Write(class_specific_distribution))
        self.play(Write(U_Edges))
        self.wait(.2)
        self.next_slide()

        observed = Tex(r"We observe the \emph{marginal} $\mathbb{P}(\mathbf{V}) = \sum_u \mathbb{P}(u) \mathbb{P}(\mathbf{V} \; | \; u)$").scale(.7).next_to(class_specific_distribution, DOWN, buff = .5)
        self.play(Write(observed))
        self.wait(.2)
        self.next_slide()


        ######################################################
        title_clustering = Text("Clustering").to_edge(UP, buff=.5)
        self.play(FullGraph.animate.align_on_border(LEFT), FadeOut(observed), TransformMatchingShapes(title, title_clustering), run_time = 1)
        axes = Axes(
            x_range=[-2, 2, .5],
            y_range=[-2, 2, .5],
            x_length=4,
            y_length=4,
            axis_config={"color": WHITE, "include_numbers": False},
            tips=False,
        ).to_edge(RIGHT, buff=1)

        self.play(Create(axes))

        # Gaussian parameters
        mean1 = [0, 1]  # Center of the first Gaussian
        mean2 = [1, -.8]  # Center of the second Gaussian
        mean3 = [-1, -.8]  # Center of the third Gaussian
        cov = [[.5, 0], [0, .5]]


        # Sample 50 points from each Gaussian distribution
        np.random.seed(42)
        points_gaussian1 = np.random.multivariate_normal(mean1, cov, 30)
        points_gaussian2 = np.random.multivariate_normal(mean2, cov, 30)
        points_gaussian3 = np.random.multivariate_normal(mean3, cov, 30)

        # Filter points to keep only those within the axes range [-2, 2] for both x and y
        filtered_points_gaussian1 = [point for point in points_gaussian1 if -2 <= point[0] <= 2 and -2 <= point[1] <= 2]
        filtered_points_gaussian2 = [point for point in points_gaussian2 if -2 <= point[0] <= 2 and -2 <= point[1] <= 2]
        filtered_points_gaussian3 = [point for point in points_gaussian3 if -2 <= point[0] <= 2 and -2 <= point[1] <= 2]

        plt_points1 = VGroup(*[
            Dot(axes.coords_to_point(point[0], point[1]), color=WHITE, radius=0.05)
            for point in filtered_points_gaussian1
        ])
        
        plt_points2 = VGroup(*[
            Dot(axes.coords_to_point(point[0], point[1]), color=WHITE, radius=0.05)
            for point in filtered_points_gaussian2
        ])

        plt_points3 = VGroup(*[
            Dot(axes.coords_to_point(point[0], point[1]), color=WHITE, radius=0.05)
            for point in filtered_points_gaussian3
        ])

        # Animate the plotting of the points
        self.play(FadeIn(plt_points1), FadeIn(plt_points2), FadeIn(plt_points3))
        self.wait(.2)
        self.next_slide()

        ############ First three circles
        def distance(point, mean):
            return np.sqrt((point[0] - mean[0])**2 + (point[1] - mean[1])**2)
        
        class1L, class2L, class3L = [], [], []
        for point in plt_points1 + plt_points2 + plt_points3:
            center = axes.point_to_coords(point.get_center())
            if distance(center, mean1) < .9:
                class1L.append(point)
            if distance(center, mean2) < .9:
                class2L.append(point)
            if distance(center, mean3) < .9:
                class3L.append(point)
        class1 = VGroup(*class1L)
        class2 = VGroup(*class2L)
        class3 = VGroup(*class3L)
        c1_circle = Circle(radius = 1, color = RED, fill_color = RED, fill_opacity=.3).move_to(axes.coords_to_point(*mean1))
        c2_circle = Circle(radius = 1, color = YELLOW, fill_color = YELLOW, fill_opacity=.3).move_to(axes.coords_to_point(*mean2))
        c3_circle = Circle(radius = 1, color = BLUE, fill_color = BLUE, fill_opacity=.3).move_to(axes.coords_to_point(*mean3))
        self.play(FadeIn(c1_circle), FadeIn(c2_circle), FadeIn(c3_circle), run_time = .5)
        self.play(class1.animate.set_color(RED), 
                  class2.animate.set_color(YELLOW), 
                  class3.animate.set_color(BLUE))
        self.wait(.2)
        self.next_slide()

        self.play(plt_points1.animate.set_color(RED), 
                  plt_points2.animate.set_color(YELLOW), 
                  plt_points3.animate.set_color(BLUE))
        self.wait(.2)
        self.next_slide()


        # Create a meshgrid for plotting the Gaussians
        x, y = np.mgrid[-2:2:.05, -2:2:.05]
        pos = np.dstack((x, y))

        # Compute the Gaussian probability densities
        rv1 = multivariate_normal(mean1, cov)
        z1 = rv1.pdf(pos)

        rv2 = multivariate_normal(mean2, cov)
        z2 = rv2.pdf(pos)

        rv3 = multivariate_normal(mean3, cov)
        z3 = rv3.pdf(pos)

        # Normalize the z values for plotting
        z1 = z1 / np.max(z1)
        z2 = z2 / np.max(z2)
        z3 = z3 / np.max(z3)

        # Plot the two Gaussians as point clouds
        points1 = VGroup(*[
            Square(side_length = .052, color=RED, fill_color = RED, fill_opacity= .7 * z1[i, j], stroke_width = 0).move_to(axes.coords_to_point(x[i, j], y[i, j]))
            for i in range(x.shape[0]) for j in range(x.shape[1]) if z1[i, j] > 0.01
        ])
        points2 = VGroup(*[
            Square(side_length = .052, color=YELLOW, fill_color = YELLOW, fill_opacity= .7 * z2[i, j], stroke_width = 0).move_to(axes.coords_to_point(x[i, j], y[i, j]))
            for i in range(x.shape[0]) for j in range(x.shape[1]) if z2[i, j] > 0.01
        ])
        points3 = VGroup(*[
            Square(side_length = .052, color=BLUE, fill_color = BLUE, fill_opacity= .7 * z3[i, j], stroke_width = 0).move_to(axes.coords_to_point(x[i, j], y[i, j]))
            for i in range(x.shape[0]) for j in range(x.shape[1]) if z3[i, j] > 0.01
        ])
        # Animate the plotting of the two Gaussian distributions
        self.play(FadeOut(c1_circle), FadeOut(c2_circle), FadeOut(c3_circle), run_time = .5)
        self.play(FadeIn(points1), FadeIn(points2), FadeIn(points3))
        self.wait(.2)
        self.next_slide()

        right_half = VGroup(points1, points2, points3, plt_points1, plt_points2, plt_points3, axes)
        left_half = FullGraph
        
        ########## Method of Moments
        self.play(right_half.animate.shift(10 * RIGHT), left_half.animate.shift(10 * LEFT))
        self.remove(left_half, right_half)
        title_overlap = Text("Algebraic Approach").to_edge(UP, buff=.5)
        self.play(TransformMatchingShapes(title_clustering, title_overlap))
        self.wait(.2)
        self.next_slide()

        parameters = Tex(r"Parameters:\\ $\mathbf{\pi} = (\pi_1, \ldots, \pi_\ell)$").scale(.7)
        moments = Tex(r"Observed Moments:\\ $m_{1}(\mathbf{V}), \ldots,  m_\ell(\mathbf{V})$").scale(.7)
        pm = VGroup(parameters, moments).arrange(RIGHT, buff=1).next_to(title, DOWN, buff=.5)
        
        self.play(Write(parameters), run_time = 1)
        self.play(Write(moments), run_time = 1)
        self.wait(.2)
        self.next_slide()

        #latex =  lambda i: r"f_" + i + r"(\mathbf{\pi}) = \mathbb{E}(m_" + i + r"(\mathbf{V}))"
        equate_moment = lambda i: MathTex(r"f_" + i + r"(\mathbf{\pi}) = \mathbb{E}(m_" + i + r"(\mathbf{V}))").scale(.7)
        #print(latex("1"))
        eqs = VGroup(equate_moment("1"), equate_moment("2"), MathTex(r"\vdots").scale(.7), equate_moment(r"\ell")).arrange(DOWN, buff = .5).next_to(pm, DOWN, buff=.5)
        for eq in eqs:
            self.play(Write(eq), run_time = .5)
            self.wait(.2)
            self.next_slide()

        moments = Text("Method of Moments").scale(.5)
        moments_cite = Paragraph('Pearson, K. (1936), "Method of Moments and Method of Maximum Likelihood", Biometrika 28(1/2), 35–59.').scale(.25)
        moment_group = VGroup(moments, moments_cite).arrange(DOWN).to_edge(DOWN, buff = 1)
        self.play(FadeIn(moment_group))
        self.wait(.2)
        self.next_slide()

        MoM_full = VGroup(pm, eqs, moment_group)
        MoM_full.generate_target()
        MoM_full.target.scale(.7).to_edge(LEFT, buff=1)
        self.play(MoveToTarget(MoM_full))
        self.wait(.2)
        self.next_slide()

        # Draw two rectangles
        
        # Labeling Classes
        LabelingClasses = Text("Labeling Classes").scale(.5)
        VGroup(plt_points1, plt_points2, plt_points3).set_color(WHITE)
        class1_copy, class2_copy, class3_copy, axes_copy = class1.copy(), class2.copy(), class3.copy(), axes.copy()
        label_classes_graph = VGroup(class1_copy, class2_copy, class3_copy, axes_copy).scale(.5).next_to(LabelingClasses, DOWN, buff=MED_SMALL_BUFF)
        IdentifyMixtures = Text("Identifying Mixtures").scale(.5).next_to(label_classes_graph, DOWN, buff=.5)
        identify_mixtures = VGroup(points1, points2, points3, plt_points1, plt_points2, plt_points3, axes).scale(.5).next_to(IdentifyMixtures, DOWN, buff=MED_SMALL_BUFF)
        LabelClasses_Rect = SurroundingRectangle(VGroup(LabelingClasses, label_classes_graph), color=WHITE, corner_radius = .1)
        IdentifyingMitures_Rect = SurroundingRectangle(VGroup(LabelClasses_Rect, IdentifyMixtures, identify_mixtures), color=WHITE, corner_radius = .1)

        inclusion_diagram = VGroup(LabelingClasses, label_classes_graph, identify_mixtures, IdentifyMixtures, LabelClasses_Rect, IdentifyingMitures_Rect).to_edge(RIGHT, buff=1.5).shift(2*UP)
        
        self.play(FadeIn(LabelingClasses), FadeIn(label_classes_graph))
        self.next_slide(loop=True)
        self.play(class1_copy.animate.set_color(RED), class2_copy.animate.set_color(YELLOW), class3_copy.animate.set_color(BLUE), run_time = .5)
        self.play(VGroup(class1_copy, class2_copy, class3_copy).animate.set_color(WHITE), run_time = .5)
        self.wait(.2)
        self.next_slide()

        self.play(FadeIn(IdentifyMixtures), FadeIn(VGroup(plt_points1, plt_points2, plt_points3, axes)))
        self.next_slide(loop=True)

        all_gaus_densities = VGroup(points1, points2, points3)
        self.play(class1_copy.animate.set_color(RED), 
                  class2_copy.animate.set_color(YELLOW), 
                  class3_copy.animate.set_color(BLUE), 
                  FadeIn(all_gaus_densities), run_time = .5)
        self.play(VGroup(class1_copy, class2_copy, class3_copy).animate.set_color(WHITE),
                  FadeOut(all_gaus_densities), run_time = .5)
        self.wait(.2)
        self.next_slide()

        self.play(FadeIn(LabelClasses_Rect), FadeIn(IdentifyingMitures_Rect),
                  class1_copy.animate.set_color(RED), 
                  class2_copy.animate.set_color(YELLOW), 
                  class3_copy.animate.set_color(BLUE), 
                  FadeIn(all_gaus_densities), run_time = .5)
        self.next_slide(loop=True)
        self.play(VGroup(class1_copy, class2_copy, class3_copy).animate.set_color(WHITE),
                  FadeOut(all_gaus_densities), run_time = .5)
        self.play(class1_copy.animate.set_color(RED), 
                  class2_copy.animate.set_color(YELLOW), 
                  class3_copy.animate.set_color(BLUE), 
                  FadeIn(all_gaus_densities), run_time = .5)
        self.wait(.2)
        self.next_slide()
        self.clear()

        title_relationship_clustering = Text("Mixtures of Relationships").to_edge(UP, buff=.5)
        self.add(title_relationship_clustering)
        xaxes = Axes(
            x_range=[-2, 2, .5],
            y_range=[-2, 2, .5],
            x_length=5,
            y_length=5,
            axis_config={"color": WHITE, "include_numbers": False},
            tips=False,
        ).next_to(title_relationship_clustering, DOWN, buff=1)

        self.play(Create(xaxes))

        # Gaussian parameters
        xmean1 = [0, 0]  # Center of the first Gaussian
        xcov1 = [[1, 0.8], [0.8, 1]]  # Positive correlation

        xmean2 = [0, 0]  # Center of the second Gaussian
        xcov2 = [[1, -0.8], [-0.8, 1]]  # Negative correlation

        # Sample 50 points from each Gaussian distribution
        np.random.seed(42)
        xpoints_gaussian1 = np.random.multivariate_normal(xmean1, xcov1, 30)
        xpoints_gaussian2 = np.random.multivariate_normal(xmean2, xcov2, 30)

        # Filter points to keep only those within the axes range [-2, 2] for both x and y
        xfiltered_points_gaussian1 = [point for point in xpoints_gaussian1 if -2 <= point[0] <= 2 and -2 <= point[1] <= 2]
        xfiltered_points_gaussian2 = [point for point in xpoints_gaussian2 if -2 <= point[0] <= 2 and -2 <= point[1] <= 2]

        # Plot the points from the first Gaussian (positive correlation, blue points)
        xplt_points1 = VGroup(*[
            Dot(xaxes.coords_to_point(point[0], point[1]), color=BLUE, radius=0.05)
            for point in xfiltered_points_gaussian1
        ])
        
        # Plot the points from the second Gaussian (negative correlation, red points)
        xplt_points2 = VGroup(*[
            Dot(xaxes.coords_to_point(point[0], point[1]), color=RED, radius=0.05)
            for point in xfiltered_points_gaussian2
        ])

        # Animate the plotting of the points
        self.play(FadeIn(xplt_points1), FadeIn(xplt_points2))
        self.wait(.2)
        self.next_slide()

        # Create a meshgrid for plotting the Gaussians
        x, y = np.mgrid[-2:2:.05, -2:2:.05]
        pos = np.dstack((x, y))

        # Compute the Gaussian probability densities
        xrv1 = multivariate_normal(xmean1, xcov1)
        xz1 = xrv1.pdf(pos)

        xrv2 = multivariate_normal(xmean2, xcov2)
        xz2 = xrv2.pdf(pos)

        # Normalize the z values for plotting
        xz1 = xz1 / np.max(xz1)
        xz2 = xz2 / np.max(xz2)

        # Plot the two Gaussians as point clouds
        xpoints1 = VGroup(*[
            Square(side_length = .052 / 4 * 5, color=BLUE, fill_color = BLUE, fill_opacity= xz1[i, j], stroke_width = 0).move_to(xaxes.coords_to_point(x[i, j], y[i, j]))
            for i in range(x.shape[0]) for j in range(x.shape[1]) if xz1[i, j] > 0.01
        ])
        xpoints2 = VGroup(*[
            Square(side_length = .052 / 4 * 5, color=RED, fill_color = RED, fill_opacity= xz2[i, j], stroke_width = 0).move_to(xaxes.coords_to_point(x[i, j], y[i, j]))
            for i in range(x.shape[0]) for j in range(x.shape[1]) if xz2[i, j] > 0.01
        ])
        # Animate the plotting of the two Gaussian distributions
        self.wait(.2)
        self.next_slide()
        self.play(FadeIn(xpoints1))
        self.wait(.2)
        self.next_slide()
        self.play(FadeIn(xpoints2))
        self.wait(.2)
        self.next_slide()
        x_graph = VGroup(xpoints1, xpoints2, xplt_points1, xplt_points2, xaxes)

        title_relationship_causal= Text("Mixtures of Causal Relationships").to_edge(UP, buff=.5)
        self.play(TransformMatchingShapes(title_relationship_clustering, title_relationship_causal))
        self.wait(.2)
        self.next_slide()
        self.play(FadeOut(x_graph))
        self.wait(.2)
        self.next_slide()

        # New DAG
        def dag(c_text, t_text, y_text, dashed_ct = False):
            c  = RectVertex(c_text).scale(1.5).shift(2* UP)
            t  = RectVertex(t_text).scale(1.5).shift(2* LEFT)
            y  = RectVertex(y_text).scale(1.5).shift(2* RIGHT)

            cy = Line(start=c.get_edge_center(DOWN) + .2 * RIGHT, end = y.get_edge_center(UP))
            cy.add_tip(tip_length = .2, tip_width=.2)
            ty = Line(start=t.get_edge_center(RIGHT), end = y.get_edge_center(LEFT))
            ty.add_tip(tip_length = .2, tip_width=.2)
            if dashed_ct:
                ct = DashedLine(start=c.get_edge_center(DOWN) + .2 * LEFT, end = t.get_edge_center(UP))
                ct.add_tip(tip_length = .2, tip_width=.2)
                tc = DashedLine(start = t.get_edge_center(UP), end=c.get_edge_center(DOWN) + .2 * LEFT)
                tc.add_tip(tip_length = .2, tip_width=.2)
                return VGroup(c, t, y, cy, ty, VGroup(ct, tc))
            else:
                ct = Line(start=c.get_edge_center(DOWN) + .2 * LEFT, end = t.get_edge_center(UP))
                ct.add_tip(tip_length = .2, tip_width=.2)
                return VGroup(c, t, y, cy, ty, ct)
            
        treatment_hetero = Text("Heterogeneity in Treatment").scale(.5)
        causal_dag_treat = dag("Drug Quality", "Drug Dosage", "Outcome", dashed_ct = True).next_to(treatment_hetero, DOWN, buff = .25).scale(.5)
        pill = lambda: SVGMobject(file_name = "svgs/pill.svg", color = PURPLE, fill_color = PURPLE).scale(.12)
        pills = VGroup(*[pill() for i in range(25)]).arrange_in_grid(5, 5, buff=.1).next_to(causal_dag_treat, DOWN, buff = .5)
        t_total = VGroup(treatment_hetero, causal_dag_treat, pills)
        
        subject_hetero = Text("Heterogeneity in Subject").scale(.5)
        causal_dag_subject = dag("Strain", "Vaccine", "Effectiveness", dashed_ct = True).next_to(subject_hetero, DOWN, buff = .25).scale(.5)
        virus = lambda: SVGMobject(file_name = "svgs/virus.svg", color = PURPLE, fill_color = PURPLE).scale(.12)
        viruses = VGroup(*[virus() for i in range(25)]).arrange_in_grid(5, 5, buff=.1).next_to(causal_dag_subject, DOWN, buff = .5)
        s_total = VGroup(subject_hetero, causal_dag_subject, viruses)

        confounding_hetero = Text("Confounded Heterogeneity").scale(.5)
        causal_dag_confounding = dag("Bacteria", "Antibiotic", "Effectiveness", dashed_ct = False).next_to(confounding_hetero, DOWN, buff = .25).scale(.5)
        bacterium = lambda: SVGMobject(file_name = "svgs/bacteria.svg", color = PURPLE, fill_color = PURPLE).scale(.12)
        bacteria = VGroup(*[bacterium() for i in range(25)]).arrange_in_grid(5, 5, buff=.1).next_to(causal_dag_confounding, DOWN, buff = .5)
        c_total = VGroup(confounding_hetero, causal_dag_confounding, bacteria)

        color_options = [RED, BLUE]
        rand_color = lambda : random.choice(color_options)

        # first dag
        t_total.move_to(ORIGIN + .75 * DOWN)
        self.play(Create(treatment_hetero))
        self.play(Create(causal_dag_treat))
        self.wait(.2)
        self.next_slide()
        self.play(FadeIn(pills))
        self.wait(.2)
        self.next_slide()
        self.play(*[pill.animate.set_color(rand_color()) for pill in pills])
        self.wait(.2)
        self.next_slide()

        # second dag
        t_total.generate_target()
        running_total = VGroup(t_total.target, s_total).arrange(RIGHT, buff = 1)
        running_total.move_to(ORIGIN + .75 * DOWN)
        self.play(MoveToTarget(t_total), Create(subject_hetero))
        self.play(Create(causal_dag_subject))
        self.wait(.2)
        self.next_slide()
        self.play(FadeIn(viruses))
        self.wait(.2)
        self.next_slide()
        self.play(*[virus.animate.set_color(rand_color()) for virus in viruses])
        self.wait(.2)
        self.next_slide()

        # third dag
        t_total.generate_target()
        s_total.generate_target()
        running_total = VGroup(t_total.target, s_total.target, c_total).arrange(RIGHT, buff = 1)
        running_total.move_to(ORIGIN + .75 * DOWN)
        self.play(MoveToTarget(s_total), MoveToTarget(t_total), Create(confounding_hetero))
        self.play(Create(causal_dag_confounding))
        self.wait(.2)
        self.next_slide()
        self.play(FadeIn(bacteria))
        self.wait(.2)
        self.next_slide()
        self.play(*[bacterium.animate.set_color(rand_color()) for bacterium in bacteria])
        self.wait(.2)
        self.next_slide()

        confounders = VGroup(*[VGroup(dag[0], dag[3], dag[5]) for dag in [causal_dag_confounding, causal_dag_subject, causal_dag_treat]])
        confounders.generate_target()
        confounders.target.set_opacity(.4)
        things_to_grey = VGroup(viruses, bacteria, pills)
        self.play(MoveToTarget(confounders), things_to_grey.animate.set_color(GRAY))
        MTEs = Text("Mixtures of Treatment Effects (MTEs)").scale(.5).next_to(s_total, DOWN, buff=.5)
        self.play(Create(MTEs))
        self.wait(.2)
        self.next_slide()

        ################ Fundimental Problem ################### 
        title_fundimental = Text("Fundamental Problem of Causal Inference").to_edge(UP, buff=.5)
        everything = VGroup(s_total, t_total, c_total, MTEs)
        self.play(TransformMatchingShapes(title_relationship_causal, title_fundimental), FadeOut(everything))
        self.wait(.2)
        self.next_slide()

        person_factual = StickFigure().to_edge(LEFT, buff=1).shift(.5 * DOWN)
        self.add(person_factual.body)

        person_treated = StickFigure(happy=True).to_edge(RIGHT, buff=7).shift(UP)

        treatment_arrow = Arrow(start = person_factual.get_center(), end=person_treated.get_center(), buff = 1)
        treated = Pill().scale(.5).next_to(treatment_arrow, .5 * UP)
        self.play(Write(treatment_arrow), FadeIn(treated), run_time = 1)
        self.play(FadeIn(person_treated), run_time = .5)
        self.wait(.2)
        self.next_slide()

        person_counterfactual = StickFigure().to_edge(LEFT, buff=1).shift(.5 * DOWN)
        self.add(person_counterfactual.body)
        person_untreated = StickFigure(happy=False).to_edge(RIGHT, buff=7).shift(2 * DOWN)
        untreatment_arrow = Arrow(start = person_counterfactual.get_center(), end=person_untreated.get_center(), buff = 1)
        untreated = Pill(False).scale(.5).next_to(untreatment_arrow, .5 * DOWN)
        self.play(Write(untreatment_arrow), FadeIn(untreated), run_time = 1)
        self.play(FadeIn(person_untreated), run_time = .5)
        self.wait(.2)
        self.next_slide()

        # Add ys
        po1 = MathTex(r'{{Y^{(1)}}}').next_to(person_treated, RIGHT, buff = 1)
        po0 = MathTex(r'{{Y^{(0)}}}').next_to(person_untreated, RIGHT, buff = 1)
        te = MathTex(r"{{\text{Treatment Effect}}}", r"=", r"{{Y^{(1)}}}", r"-", r"{{Y^{(0)}}}").scale(.7).to_edge(RIGHT, buff=1).shift(.5 * DOWN)
        potential_outcomes = Text("Potential Outcomes").scale(.5)
        po_cite = VGroup(
            Paragraph("Neyman, Jerzy (1923). Sur les applications de la theorie des probabilites\n aux experiences agricoles: Essai des principes. Master's Thesis.").scale(.25),
            Paragraph('Rubin, Donald (1974). "Estimating Causal Effects of Treatments in Randomized\n and Nonrandomized Studies". J. Educ. Psychol. 66 (5): 688–701 [p. 689].').scale(.25)
        ).arrange(DOWN)
        po_section = VGroup(potential_outcomes, po_cite).arrange(DOWN).to_edge(RIGHT, buff = .25).shift(.5 * DOWN)
        self.play(Write(po1), Write(po0), run_time = 1)
        self.play(Write(potential_outcomes), FadeIn(po_cite))
        self.wait(.2)
        self.next_slide()
        self.play(FadeOut(po_section), run_time=.5)
        self.play(Write(te), run_time = 1)
        self.wait(.2)
        self.next_slide()

        # Cant see both
        top = VGroup(po1, treatment_arrow, person_treated, te[2])
        bot = VGroup(po0, untreatment_arrow, person_untreated, te[4])
        bot.generate_target()
        bot.target.set_opacity(.3)
        self.play(MoveToTarget(bot), run_time = .5)
        self.wait(.2)
        self.next_slide()

        top.generate_target()
        top.target.set_opacity(.3)
        bot.generate_target()
        bot.target.set_opacity(1)
        self.play(MoveToTarget(top), MoveToTarget(bot), run_time =.5)
        self.wait(.2)
        self.next_slide()

        top.generate_target()
        top.target.set_opacity(1)
        self.play(MoveToTarget(top), run_time = .5)
        self.wait(.2)
        self.next_slide()


        # Separate
        untreatment_arrow_2 = always_redraw(lambda: Arrow(start = person_counterfactual.get_center(), end=person_untreated.get_center(), buff = 1))
        treatment_arrow_2 = always_redraw(lambda: Arrow(start = person_factual.get_center(), end=person_treated.get_center(), buff = 1))
        self.add(untreatment_arrow_2, treatment_arrow_2)
        self.remove(untreatment_arrow, treatment_arrow)
        person_factual.remove(person_factual.head)
        person_factual.generate_target()
        person_factual.target.shift(1.5 * UP)
        person_counterfactual.remove(person_counterfactual.head)
        person_counterfactual.generate_target()
        person_counterfactual.target.shift(1.5 * DOWN)
        treated.generate_target()
        treated.target.shift(.2 * UP)
        untreated.generate_target()
        untreated.target.shift(.2 * DOWN)
        self.play(MoveToTarget(person_factual), MoveToTarget(person_counterfactual), MoveToTarget(treated), MoveToTarget(untreated), run_time = 1)
        self.wait(.2)
        self.next_slide()

        # Groups
        group_of_sticks = StickFigure.group_of_sticks(n=9, width=3, scale=.3, v_sep=.7, h_sep=.5, happy_prob =.75)
        group_of_sticks2 = StickFigure.group_of_sticks(n=9, width=3, scale=.3, v_sep=.7, h_sep=.5, happy_prob = .25)
        group_of_sticks.move_to(person_factual)
        group_of_sticks_treated = group_of_sticks.copy().move_to(person_treated)
        group_of_sticks2.move_to(person_counterfactual)
        group_of_sticks.move_to(person_factual)
        group_of_sticks_untreated = group_of_sticks2.copy().move_to(person_untreated)
        apo1 = MathTex(r'\mathbb{E}[{{Y^{(1)}}}]').next_to(person_treated, RIGHT, buff = 1)
        apo0 = MathTex(r'\mathbb{E}[{{Y^{(0)}}}]').next_to(person_untreated, RIGHT, buff = 1)
        self.play(FadeOut(person_factual), FadeOut(person_counterfactual), FadeOut(person_treated), FadeOut(person_untreated))
        self.play(TransformMatchingTex(po1, apo1), TransformMatchingTex(po0, apo0),
                   *[FadeIn(stick.body) for stick in group_of_sticks], 
                   *[FadeIn(stick.body) for stick in group_of_sticks2], 
                   FadeIn(group_of_sticks_treated), FadeIn(group_of_sticks_untreated))
        ate = MathTex(r"{{\text{Avg Treatment Effect}}}", r"=", r"\mathbb{E}[{{Y^{(1)}}}]", r"-", r"\mathbb{E}[{{Y^{(0)}}}]").to_edge(RIGHT, buff=1).scale(.7).move_to(te)
        self.play(TransformMatchingTex(te, ate))
        self.wait(.2)
        self.next_slide()
        
        # ###Randomization, or Covariate Adjustments
        treated_red_blues = [RED.interpolate(BLUE, random.random()) for i in range(9)]
        untreated_red_blues = [RED.interpolate(BLUE, random.random()) for i in range(9)]
        CovAdjustments = Text("Adjustment Sets").scale(.5)
        CovAdjustments_cite = Paragraph("Pearl, J. (2009). Causality. Cambridge university press.").scale(.25)
        full_cite = VGroup(CovAdjustments, CovAdjustments_cite).arrange(DOWN, center=True).to_corner(DR, buff = .25)
        self.play(*[stick.animate.set_color(c) for stick, c in zip(group_of_sticks, treated_red_blues)],
                    *[stick.animate.set_color(c) for stick, c in zip(group_of_sticks_treated, treated_red_blues)],
                    *[stick.animate.set_color(c) for stick, c in zip(group_of_sticks2, untreated_red_blues)],
                    *[stick.animate.set_color(c) for stick, c in zip(group_of_sticks_untreated, untreated_red_blues)],
                    FadeIn(full_cite))
        self.wait(.2)
        self.next_slide()

        ################ (RIGHT) Causal Forests are Clustering, (1) only recovers causal quantities clustering space is an adjustment set (unconfoundedness) (2) Cant handle overlap ###################
        everything = VGroup(full_cite, ate, apo1, apo0, group_of_sticks, group_of_sticks2, group_of_sticks_treated, group_of_sticks_untreated, treated, untreated, treatment_arrow_2, untreatment_arrow_2)
        title = self.change_title("Causal Clusters", title_fundimental, everything)
        self.play(Create(xaxes))
        pts = np.random.uniform(0, 1, (100,2))
        peoples = VGroup(*[
            StickFigure().set_color(RED.interpolate(BLUE, point[0])).scale(.1).move_to(xaxes.coords_to_point(point[0]*4 - 2, point[1]*4 -2))
            for point in pts
        ])
        self.play(FadeIn(peoples))
        self.wait(.2)
        self.next_slide()

        mean1 = [-1,-1]
        mean2 = [1,1]
        c2_circle = Circle(radius = 1, color = BLUE, fill_color = RED.interpolate(BLUE,.75), fill_opacity=.3).move_to(xaxes.coords_to_point(*mean2))
        c1_circle = Circle(radius = 1, color = RED, fill_color = RED.interpolate(BLUE,.25), fill_opacity=.3).move_to(xaxes.coords_to_point(*mean1))
        whole_graph = VGroup(c1_circle, c2_circle, peoples, xaxes)
        CausalForest = Text("Causal Forests for HTEs").scale(.5)
        CF_cite = Paragraph('S. Wagner, S. Athey (2018). "Estimation and Inference of Heterogeneous Treatment\n Effects using Random Forests." Journal of the American Statistical Association.').scale(.25)
        full_cite_cf = VGroup(CausalForest, CF_cite).arrange(DOWN, center=True).to_edge(RIGHT, buff = .75).shift(1.5 * UP)
        self.play(Create(c1_circle), Create(c2_circle))
        self.wait(.2)
        self.next_slide()
        whole_graph.generate_target()
        whole_graph.target.to_edge(LEFT, buff = .75)
        self.play(FadeIn(full_cite_cf), MoveToTarget(whole_graph))
        self.wait(.2)
        self.next_slide()

        U = Vertex("U", observed=False).shift(.5 * UP)
        T = Vertex("T").shift(DOWN + LEFT)
        Y = Vertex("Y").shift(DOWN + RIGHT)
        UT = Edge(U, T)
        UY = Edge(U, Y)
        TY = Edge(T, Y)
        canonical_dag = VGroup(U, T, Y, UT, UY, TY).scale(.75)

        need_1 = VGroup(Text("1. Latent class is in all adjustment sets.").scale(.5), canonical_dag).arrange(DOWN, buff = .5)
        need_2 = Text("2. Needs disjoint classes (like clustering).").scale(.5)
        needs = VGroup(need_1, need_2).arrange(DOWN, buff = .5).next_to(full_cite_cf, DOWN, buff = 1)
        self.play(Create(need_1))
        self.wait(.2)
        self.next_slide()
        c1_circle.generate_target()
        c2_circle.generate_target()
        c2_circle.target.shift(DOWN + LEFT)
        c1_circle.target.shift(UP + RIGHT)
        self.play(MoveToTarget(c1_circle), MoveToTarget(c2_circle), Create(need_2))
        self.wait(.2)
        self.next_slide()
        self.clear()

        title = Text("Tensor Methods").to_edge(UP, buff=.5)
        self.add(title)

        V1 = Vertex(label = r'V_1')
        V2 = Vertex(label = r'V_2').shift(3 * RIGHT)
        V3 = Vertex(label = r'V_3').shift(6 * RIGHT)
        V = VGroup(V1, V2, V3)
        U = Vertex(label=r'U', observed=False).next_to(V, 2.5 * UP)

        U_Edges = VGroup(*[Edge(U, V, observed = False) for V in V])
        FullGraph = VGroup(V, U, U_Edges)
        FullGraph.next_to(title, 3.5 * DOWN)
        self.play(Create(FullGraph))
        self.wait(.2)
        self.next_slide()

        # Vectorize
        U_vec = MobjectMatrix([[MathTex(r"\mathbb{P}(u^{(1)})").scale(.3), MathTex(r"\ldots").scale(.3), MathTex(r"\mathbb{P}(u^{(k)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, h_buff=0.7).next_to(U, UP)
        def P_vec(number, obj):
            row1 = [MathTex(r"\mathbb{E}(V_" + str(number) + r"=1" + r"|" + r" u^{(1)}) \ldots").scale(.3), 
            MathTex(r"\mathbb{E}(V_" + str(number) + r"=n_" + str(number) + r"|" + r" u^{(1)})").scale(.3)]
            row2 = [MathTex(r"\vdots").scale(.3), 
            MathTex(r"\vdots").scale(.3)]
            row3 = [MathTex(r"\mathbb{E}(V_" + str(number) + r"=1" + r"|" + r" u^{(k)}) \ldots").scale(.3),
            MathTex(r"\mathbb{E}(V_" + str(number) + r"=n_"+ str(number) + r"|" + r" u^{(k)})").scale(.3)]
            return MobjectMatrix([row1, row2, row3], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.5).next_to(obj, DOWN)
        
        col_vecs = VGroup(*[P_vec(num+1, v) for num,v in enumerate(V)])
        self.play(Write(U_vec), Write(col_vecs))
        self.wait(.2)
        self.next_slide()

        moments = Tex(r"Obervable Moments: $\mathbb{E}[V_1=i, V_2=j, V_3=\ell]$").scale(.5)
        Thm = Tex(r"Generic identifiability when $\min(n_1, k) + \min(n_2, k) + \min(n_3, k) \geq 2k + 2$").scale(.5)
        citation1 = Paragraph("J. Kruskal (1977). Three-way arrays: Rank and uniqueness of trilinear decompositions, with application\n to arithmetic complexity and statistics. Linear Algebra Appl.").scale(.25)
        citation2 = Paragraph("E. Allman, C. Matias, J. Rhodes (2009). Identifiability of parameters in latent structure models with many\n observed variables. Anals of Statistics.").scale(.25)
        citation3 = Paragraph("A. Anandkumar, R. Ge, D. Hsu, S. Kakade, M. Telgarsky (2014). Tensor decompositions for learning latent\n variable models. Journal of Machine Learning Research.").scale(.25)
        ident_result = VGroup(moments, Thm, citation1, citation2, citation3)
        ident_result.arrange(DOWN, center=True).next_to(col_vecs, DOWN).shift(.1 * DOWN)
        for t in ident_result:
            self.play(FadeIn(t))
            self.wait(.2)
            self.next_slide()

        everything = VGroup(ident_result, U_vec, col_vecs)
        title = self.change_title("Tensor Methods for MTEs", title, everything)
        (T, Y) = self.duplicate(V2, U_Edges[1], U, ["T", "Y"])
        TY = Edge(T, Y)
        self.play(Create(TY))
        self.wait(.2)
        self.next_slide()

        CATEs = Tex(r"CATEs: $\mathbb{E}[Y^{(1)} - Y^{(0)} \; | \; u] = \mathbb{E}[Y \; | \; T=1, u] - \mathbb{E}[Y \; | \; T=0, u]$").scale(.75)
        ClearCite = Paragraph("S.L. Gordon, B. Mazaheri, Y. Rabani, and L. Schulman (2023). \nCausal Inference Despite Limited Global Confounding via Mixture Models. \nIn Conference on Causal Learning and Reasoning (pp. 574-601). PMLR.").scale(.4)
        DoesntMatter1 = Tex(r"$\mathbb{E}[V_1 \;|\; u], \mathbb{E}[V_3 \;|\; u]$ are useless parameters!").scale(.5)
        DoesntMatter2 = Tex(r"Only the \emph{difference} between potential outcomes matters!").scale(.5)
        CATEs_and_DMs = VGroup(CATEs, ClearCite, DoesntMatter1, DoesntMatter2).arrange(DOWN, buff=.5).next_to(FullGraph, DOWN, buff=.5)
        self.play(Create(CATEs))
        self.wait(.2)
        self.next_slide()
        self.play(Create(ClearCite))
        self.wait(.2)
        self.next_slide()
        self.play(Create(DoesntMatter1), VGroup(V1, V3).animate.set_color(YELLOW))
        self.wait(.2)
        self.next_slide()
        self.play(Create(DoesntMatter2), VGroup(V1, V3).animate.set_color(WHITE), VGroup(T, Y, TY).animate.set_color(YELLOW))
        self.wait(.2)
        self.next_slide()
        self.play(VGroup(T, Y, TY).animate.set_color(WHITE), run_time = .5)
        everything = VGroup(CATEs_and_DMs)
        title = self.change_title("Synthetic Potential Outcomes", title, everything)
        self.wait(.2)
        self.next_slide()
        X = Vertex(label = "X").move_to(V3)
        Z = Vertex(label = "Z").move_to(V1)
        XY = Edge(X, Y)
        ZT = Edge(Z, T)
        self.play(TransformMatchingShapes(V1, Z), TransformMatchingShapes(V3, X), Create(XY), Create(ZT))
        self.wait(.2)
        self.next_slide()
        conds = Text("Conditions").scale(.7).next_to(FullGraph, DOWN, buff = 1)
        condition1 = MathTex(r"X \perp \!\!\! \perp T \;|\; U")
        condition2 = MathTex(r"Y \perp \!\!\! \perp Z \;|\; T, U")
        conditions = VGroup(condition1, condition2).arrange(DOWN, buff = .1).next_to(conds, DOWN)
        cond_box = VGroup(conds, conditions)
        self.play(Create(cond_box))
        self.wait(.2)
        self.next_slide()
        cond_box.generate_target()
        cond_box.target.scale(.7).to_corner(UL, buff = .5).shift(DOWN)
        self.play(MoveToTarget(cond_box))
        self.wait(.2)
        self.next_slide()

        #Add vectors
        # Vectorize
        U_vec = MobjectMatrix([[MathTex(r"\mathbb{P}(u^{(0)})").scale(.3), MathTex(r"\mathbb{P}(u^{(1)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, h_buff=0.7).next_to(U, UP)
        P_vec = lambda ob: MobjectMatrix([[MathTex(r"\mathbb{E}(" + ob.get_label_text() + r"|u^{(0)})").scale(.3)], [MathTex(r"\mathbb{E}(" + ob.get_label_text() + r"|u^{(1)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5).next_to(ob, DOWN)
        def E_vec(lb, cond=""):
            return MobjectMatrix([[MathTex(r"\mathbb{E}(" + lb + r"|" + cond + r" u^{(0)})").scale(.3)], [MathTex(r"\mathbb{E}(" + lb + r"|" + cond + r" u^{(1)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5)
        
        col_vecs = VGroup(*[P_vec(v) for v in [Z, Y, X]])
        self.play(FadeIn(U_vec), FadeIn(col_vecs))
        self.wait(.2)
        self.next_slide()

        Wanted_label = Text("Wanted:").set_color(GREEN).scale(.7)
        wanted_eq = VGroup(
            MathTex(r"\mathbb{E}(Y^{(t)}) = ").scale(.75),
            U_vec.copy().scale(1.2),
            E_vec("Y", cond = " t,").scale(1.2)
        ).arrange(RIGHT).set_color(GREEN)
        wanted = VGroup(Wanted_label, wanted_eq).arrange(DOWN, buff = .5).next_to(FullGraph, DOWN, buff = 2)

        self.play(Create(wanted))
        self.wait(.2)
        self.next_slide()
        wanted.generate_target()
        wanted.target.scale(.7).to_corner(UR, buff = .5).shift(DOWN)
        self.play(MoveToTarget(wanted))
        self.wait(.2)
        self.next_slide()

        #Cond and Intervening
        exp_label = Text("Expectation:").scale(.7)
        exp_eq = VGroup(
            MathTex(r"\mathbb{E}(Y) = ").scale(.75),
            U_vec.copy().scale(1.2),
            E_vec("Y", cond = "").scale(1.2)
        ).arrange(RIGHT)
        exp = VGroup(exp_label, exp_eq).arrange(DOWN, buff = .5).next_to(FullGraph, DOWN, buff = 2)
        self.play(Create(exp))

        # Turn green
        self.wait(.2)
        self.next_slide()
        self.play(exp_eq[1].animate.set_color(GREEN), U.animate.set_color(GREEN), U_vec.animate.set_color(GREEN))

        # Condition
        self.next_slide(loop = True)
        U_vec2 = MobjectMatrix([[MathTex(r"\mathbb{P}(u^{(0)} | t)").scale(.3), MathTex(r"\mathbb{P}(u^{(1)}|t)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, h_buff=0.7).move_to(U_vec)
        Y_vec2 = E_vec("Y", cond = " t,").set_color(GREEN).move_to(col_vecs[1])
        Z_vec2 = E_vec("Z", cond = " t,").move_to(col_vecs[0])

        new_exp_label = Text("Conditional Expectation:").scale(.7)
        new_exp_eq = VGroup(
            MathTex(r"\mathbb{E}(Y | t) = ").scale(.75),
            U_vec2.copy().scale(1.2),
            E_vec("Y", cond = " t, ").scale(1.2).set_color(GREEN)
        ).arrange(RIGHT)
        new_exp = VGroup(new_exp_label, new_exp_eq).arrange(DOWN, buff = .5).next_to(FullGraph, DOWN, buff = 2)
        self.play(T.condition(), 
                  TransformMatchingShapes(U_vec, U_vec2), 
                  TransformMatchingShapes(col_vecs[1], Y_vec2),
                  TransformMatchingShapes(col_vecs[0], Z_vec2),
                  TransformMatchingShapes(exp_label, new_exp_label),
                  TransformMatchingShapes(exp_eq[0], new_exp_eq[0]),
                  TransformMatchingShapes(exp_eq[2], new_exp_eq[2]),
                  exp_eq[1].animate.set_color(WHITE), 
                  U.animate.set_color(WHITE),
                  Y.animate.set_color(GREEN))
        self.next_slide()
        self.play(condition1.animate.set_color(YELLOW))
        self.wait(.2)
        self.next_slide()
        self.play(condition1.animate.set_color(WHITE), FadeOut(VGroup(new_exp_label, new_exp_eq[0], exp_eq[1], new_exp_eq[2])))
        self.wait(.2)
        self.next_slide()
        # Add X1, X2, Z1, Z2 columns
        X1_vec = E_vec(r"X_1").move_to(col_vecs[2])
        X2_vec = E_vec(r"X_2").next_to(X1_vec, RIGHT, buff=1)
        Z1_vec = E_vec(r"Z_1", cond = " t, ").next_to(col_vecs[0], LEFT, buff=1)
        Z2_vec = E_vec(r"Z_2", cond = " t, ").next_to(Z1_vec, RIGHT, buff=1)
        self.play(TransformMatchingShapes(Z_vec2, Z2_vec),
                  TransformMatchingShapes(col_vecs[2], X1_vec),
                  FadeIn(X2_vec),
                  FadeIn(Z1_vec))
        
        # Color and linear comb
        self.wait(.2)
        self.next_slide()
        X1_vec.generate_target()
        X1_vec.target.set_color(YELLOW)
        X2_vec.generate_target()
        X2_vec.target.set_color(BLUE)
        self.play(*[MoveToTarget(t) for t in [X1_vec, X2_vec]])

        copyY = Y_vec2.copy()
        copyY.generate_target()
        copyX1 = X1_vec.copy()
        copyX1.generate_target()
        copyX2 = X2_vec.copy()
        copyX2.generate_target()
        want_alphas = VGroup(
            copyY.target,
            MathTex(r"= \alpha_1"),
            copyX1.target,
            MathTex(r"+ \alpha_2"),
            copyX2.target
        ).arrange(RIGHT)
        want_alpha_eq_structure = VGroup(want_alphas[1], want_alphas[3])
        moment_matching_1 = MathTex(r"\mathbb{E}(Y Z_1|t) = \alpha_1 \mathbb{E}(X_1 Z_1|t) + \alpha_2 \mathbb{E}(X_2 Z_1|t)").scale(.8)
        moment_matching_2 = MathTex(r"\mathbb{E}(Y Z_2|t) = \alpha_1 \mathbb{E}(X_1 Z_2|t) + \alpha_2 \mathbb{E}(X_2 Z_2|t)").scale(.8)
        moment_matching = VGroup(moment_matching_1, moment_matching_2).arrange(DOWN, center=True)
        alpha_derive = VGroup(want_alphas, moment_matching).arrange(DOWN, center=True).next_to(col_vecs, DOWN)
        self.play(Write(want_alpha_eq_structure), MoveToTarget(copyY), MoveToTarget(copyX1), MoveToTarget(copyX2))

        # Moment matching
        self.play(Write(moment_matching))
        self.wait(.2)
        self.next_slide()
        self.play(condition2.animate.set_color(YELLOW))
        self.wait(.2)
        self.next_slide()

        # Rearrange as matrix equations
        moment_matching_matrix = VGroup(
            MobjectMatrix([[MathTex(r"\alpha_1").scale(.3)], [MathTex(r"\alpha_2").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = .7),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1 Z_1|t)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_1|t)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z_2|t)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_2|t)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y Z_1|t)").scale(.3)], [MathTex(r"\mathbb{E}(Y Z_2|t)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1)
        ).arrange(RIGHT)
        new_wanted = VGroup(
            MathTex(r"\mathbb{E}(Y^{(t)})", color = GREEN),
            MathTex(r"= \alpha_1"),
            MathTex(r"\mathbb{E}(X_1)", color=YELLOW),
            MathTex(r"+ \alpha_2"),
            MathTex(r"\mathbb{E}(X_2)", color = BLUE),
        ).arrange(RIGHT)
        final_eqs = VGroup(moment_matching_matrix, new_wanted).arrange(DOWN, center=True).center().to_edge(DOWN, buff = 1)
        inverse = MathTex(r"-1").scale(.3).next_to(moment_matching_matrix[2], RIGHT, buff=0.05).shift(.3 * UP)
        self.play(FadeOut(want_alphas), 
                  FadeOut(VGroup(copyX1, copyX2, copyY)), 
                  FadeIn(inverse), 
                  TransformMatchingShapes(moment_matching, moment_matching_matrix), 
                  TransformMatchingShapes(wanted_eq.copy(), new_wanted),
                  condition2.animate.set_color(WHITE))
        self.wait(.2)
        self.next_slide()


        moment_matching_matrix_a = VGroup(
            MobjectMatrix([[MathTex(r"\alpha_1").scale(.3)], [MathTex(r"\alpha_2").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1 Z_1|T=1)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_1|T=1)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z_2|T=1)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_2|T=1)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y Z_1|T=1)").scale(.3)], [MathTex(r"\mathbb{E}(Y Z_2|T=1)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4)
        ).arrange(RIGHT)
        moment_matching_matrix_b = VGroup(
            MobjectMatrix([[MathTex(r"\beta_1").scale(.3)], [MathTex(r"\beta_2").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1 Z_1|T=0)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_1|T=0)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z_2|T=0)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_2|T=0)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y Z_1|T=0)").scale(.3)], [MathTex(r"\mathbb{E}(Y Z_2|T=0)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4)
        ).arrange(RIGHT)
        ATE_eq = VGroup(
            MathTex(r"\mathbb{E}(Y^{(1)}) - \mathbb{E}(Y^{(0)})"),
            MathTex(r"= (\alpha_1 - \beta_1)"),
            MathTex(r"\mathbb{E}(X_1)", color=YELLOW),
            MathTex(r"+ (\alpha_2 - \beta_2)"),
            MathTex(r"\mathbb{E}(X_2)", color = BLUE),
        ).arrange(RIGHT)
        final_calc = VGroup(ATE_eq, moment_matching_matrix_a, moment_matching_matrix_b).arrange(DOWN, center=True).center()
        inverse_a = MathTex(r"-1").scale(.3).next_to(moment_matching_matrix_a[2], RIGHT, buff=0.05).shift(.3 * UP)
        inverse_b = MathTex(r"-1").scale(.3).next_to(moment_matching_matrix_b[2], RIGHT, buff=0.05).shift(.3 * UP)
        main_highlight = BackgroundRectangle(final_calc, color = WHITE, stroke_width=8, stroke_opacity=1, fill_opacity=1, fill_color = BLACK, buff=.3)
        moment_matching_copy = moment_matching_matrix.copy()
        self.play(FadeIn(main_highlight),
                  TransformMatchingShapes(moment_matching_matrix, moment_matching_matrix_a),
                  TransformMatchingShapes(moment_matching_copy, moment_matching_matrix_b),
                  FadeIn(inverse_a),
                  FadeIn(inverse_b),
                  FadeIn(ATE_eq),
                  FadeOut(VGroup(new_wanted, inverse)))
        
        self.wait(.2)
        self.next_slide()
        # Simplify to gamma
        simple_ATE_eq = VGroup(
            MathTex(r"\mathbb{E}(R)"),
            MathTex(r"= (\gamma_1)"),
            MathTex(r"\mathbb{E}(X_1)", color=YELLOW),
            MathTex(r"+ (\gamma_2)"),
            MathTex(r"\mathbb{E}(X_2)", color = BLUE),
        ).arrange(RIGHT).move_to(ATE_eq)
        final_calc = VGroup(simple_ATE_eq, moment_matching_matrix_a, moment_matching_matrix_b)
        self.play(*[TransformMatchingShapes(part, new_part) for part, new_part in zip(ATE_eq, simple_ATE_eq)])

        self.wait(.2)
        self.next_slide()
        proceedure = VGroup(main_highlight, moment_matching_matrix_a, moment_matching_matrix_b, inverse_a, inverse_b, simple_ATE_eq)
        identifiability = Tex(r"Identifiability $\rightarrow$ Matrix Singularity", color=WHITE).scale(.8)
        stability = Tex(r"Stability $\rightarrow$ Matrix Condition Number", color=WHITE).scale(.8)
        cite = Paragraph("W. Miao, X. Shi., Y. Li and E.J. Tchetgen (2024). A confounding bridge approach for double negative\n control inference on causal effects. Statistical Theory and Related Fields, pp.1-12.").scale(.4)
        main_points = VGroup(identifiability, stability, cite)
        main_points.arrange(DOWN)
        proceedure.generate_target()
        proceedure.target.next_to(title, DOWN, buff = .5)
        main_points.to_edge(DOWN, buff = 1)
        self.play(MoveToTarget(proceedure))
        for t in main_points:
            self.play(Write(t))
            self.wait(.2)
        self.next_slide()

        self.play(FadeOut(main_points), FadeOut(proceedure))
        self.wait(.2)
        self.next_slide()

        wanted_eq_2 = VGroup(
            MathTex(r"\mathbb{E}(Y^{(t)} \odot R) = ").scale(.75),
            U_vec.copy().scale(1.2),
            E_vec("Y", cond = " t,").scale(1.2),
            MathTex(r"\odot").scale(.75),
            E_vec("R").scale(1.2),
        ).arrange(RIGHT).scale(.6).move_to(wanted_eq).shift(.7*LEFT).set_color(GREEN)
        self.play(*[TransformMatchingShapes(wanted_eq[i], wanted_eq_2[i]) for i in range(3)], FadeIn(wanted_eq_2[3]), FadeIn(wanted_eq_2[4]))
        self.wait(.2)
        self.next_slide()

        # Make X1 and X2 into y
        R_vec = E_vec("R").move_to(X1_vec).set_color(GREEN)
        self.play(TransformMatchingShapes(VGroup(X1_vec, X2_vec), R_vec))
        self.wait(.2)
        self.next_slide()
        # Update conditions
        new_condition1 = MathTex(r"X \perp \!\!\! \perp (T,Y) \;|\; U").scale(.7).move_to(condition1)
        self.play(FadeOut(XY), TransformMatchingShapes(condition1, new_condition1))

        ##
        self.wait(.2)
        self.next_slide()
        exp_label = Text("Conditional Expectation:").scale(.7)
        exp_eq = VGroup(
            MathTex(r"\mathbb{E}(R \odot Y | t) = ").scale(.7),
            MathTex(r"\gamma_1 \mathbb{E}(X_1 \odot Y | t) + \gamma_2\mathbb{E}(X_2 \odot Y | t) = ").scale(.7),
            U_vec2.copy().scale(1),
            E_vec("Y", cond = " t, ").scale(1).set_color(GREEN),
            MathTex(r"\odot").scale(.75),
            E_vec("R", cond = " ").scale(1).set_color(GREEN)
        ).arrange(RIGHT)
        exp = VGroup(exp_label, exp_eq).arrange(DOWN, buff = .5).next_to(FullGraph, DOWN, buff = 2)
        self.play(Create(exp))
        self.wait(.2)
        self.next_slide()
        self.play(FadeOut(exp_eq), FadeOut(exp_label))

        param_vec = lambda p, n: MobjectMatrix([[MathTex(p + r"^{(" + str(n) + r")}_1").scale(.3)], [MathTex(p + r"^{(" + str(n) + r")}_2").scale(.3)]],
                                                bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4)
        moment_matching_matrix_a = VGroup(
            param_vec(r"\alpha", 2),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1 Z_1|T=1)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_1|T=1)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z_2|T=1)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_2|T=1)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y X_1 Z_1|T=1)").scale(.3), MathTex(r"\mathbb{E}(Y X_2 Z_1|T=1)").scale(.3)], [MathTex(r"\mathbb{E}(Y X_1 Z_2|T=1)").scale(.3), MathTex(r"\mathbb{E}(Y X_2 Z_2|T=1)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            param_vec(r"\gamma", 2)
        ).arrange(RIGHT)
        moment_matching_matrix_b = VGroup(
            param_vec(r"\beta", 2),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1 Z_1|T=0)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_1|T=0)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z_2|T=0)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_2|T=0)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y X_1 Z_1|T=0)").scale(.3), MathTex(r"\mathbb{E}(Y X_2 Z_1|T=0)").scale(.3)], [MathTex(r"\mathbb{E}(Y X_1 Z_2|T=0)").scale(.3), MathTex(r"\mathbb{E}(Y X_2 Z_2|T=0)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            param_vec(r"\gamma", 2)
        ).arrange(RIGHT)
        gamma_def = VGroup(
            param_vec(r"\gamma", 2),
            MathTex("="),
            param_vec(r"\alpha", 2),
            MathTex("-"),
            param_vec(r"\beta", 2)
        ).arrange(RIGHT)
        ATE_eq = VGroup(
            MathTex(r"\mathbb{E}(R^{\odot 2})"),
            MathTex(r"= \gamma^{(2)}_1"),
            MathTex(r"\mathbb{E}(X_1)", color=YELLOW),
            MathTex(r"+ \gamma^{(2)}_2"),
            MathTex(r"\mathbb{E}(X_2)", color = BLUE),
        ).arrange(RIGHT)
        final_calc = VGroup(ATE_eq, gamma_def, moment_matching_matrix_a, moment_matching_matrix_b).arrange(DOWN, center=True).center()
        inverse_a = MathTex(r"-1").scale(.3).next_to(moment_matching_matrix_a[2], RIGHT, buff=0.05).shift(.3 * UP)
        inverse_b = MathTex(r"-1").scale(.3).next_to(moment_matching_matrix_b[2], RIGHT, buff=0.05).shift(.3 * UP)
        main_highlight = BackgroundRectangle(final_calc, color = WHITE, stroke_width=8, stroke_opacity=1, fill_opacity=1, fill_color = BLACK, buff=.3)
        self.play(FadeIn(main_highlight),
                  FadeIn(final_calc),
                  FadeIn(inverse_a),
                  FadeIn(inverse_b))
        self.wait(.2)
        self.next_slide()
        self.clear()

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
        self.clear()

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







