from manim_slides import Slide, ThreeDSlide
import random
from manim import *
from GraphManim import *
from scipy.stats import multivariate_normal
from icons import *


class CausalMixtures(Slide):
    def change_title(self, new_title, old, everything):
        new = Text(new_title).to_edge(UP, buff=.5)
        self.play(TransformMatchingShapes(old, new), FadeOut(everything))
        return new

    def construct(self): 
        ############################################### PLOT AN X####
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

