from manim_slides import Slide, ThreeDSlide
from manim import *
from GraphManim import *
from scipy.stats import multivariate_normal

class MixtureModels(Slide):
    def construct(self):
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
        

