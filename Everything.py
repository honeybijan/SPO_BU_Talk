from manim_slides import Slide, ThreeDSlide
import random
from manim import *
from GraphManim import *
from scipy.stats import multivariate_normal
from icons import *
from CoinFlippingClasses import *
from GraphManim import *

P_vec = lambda ob: MobjectMatrix([[MathTex(r"\mathbb{E}(" + ob.get_label_text() + r"|u^{(0)})").scale(.3)], [MathTex(r"\mathbb{E}(" + ob.get_label_text() + r"|u^{(1)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5).next_to(ob, DOWN)

def fade_to_gray(list_of_edges):
    for edge in list_of_edges:
        edge.generate_target()
        edge.target.set_opacity(.3)
    return [MoveToTarget(edge) for edge in list_of_edges]

def fade_back(list_of_edges):
    for edge in list_of_edges:
        edge.generate_target()
        edge.target.set_opacity(1)
    return [MoveToTarget(edge) for edge in list_of_edges]

def E_vec(lb, cond=""):
    return MobjectMatrix([[MathTex(r"\mathbb{E}(" + lb + r"|" + cond + r" u^{(0)})").scale(.3)], [MathTex(r"\mathbb{E}(" + lb + r"|" + cond + r" u^{(1)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5)

def cite_group(*args):
    return VGroup(*[Text(t).scale(.3) for t in args]).arrange(DOWN, buff = .05)

def rand_col(red_prob):
    if random.random() < red_prob:
        return RED
    else:
        return BLUE

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

class Everything(Slide):

    def change_title(self, new_title, old, everything = None):
        new = Text(new_title).to_edge(UP, buff=.5)
        if old:
            title_anim = TransformMatchingShapes(old, new)
        else:
            title_anim = Write(new)
        if everything:
            self.play(title_anim, FadeOut(everything))
        else:
            self.play(title_anim)
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
        return (new_left, new_right, new_left_arrow, new_right_arrow)
    
    def intro(self):
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

    def mushrooms_and_turkeys(self):
        title = Text("How do we group things?").to_edge(UP, buff=.5)
        self.play(FadeIn(title))
        edible_mushroom = SVGMobject(file_name = "svgs/mushroom-edible.svg", color = LIGHT_BROWN, fill_color = LIGHT_BROWN).scale(.5)
        mario_mushroom = SVGMobject(file_name = "svgs/mushroom-mario.svg", color = RED, fill_color = RED).scale(.5)
        live_turkey = SVGMobject(file_name = "svgs/turkey-live.svg", color = RED, fill_color = RED).scale(.5)
        cooked_turkey = SVGMobject(file_name = "svgs/turkey-edible.svg", color = LIGHT_BROWN, fill_color = LIGHT_BROWN).scale(.5)
        turkeys = VGroup(live_turkey, cooked_turkey).arrange(DOWN, buff = 1)
        mushrooms = VGroup(mario_mushroom, edible_mushroom).arrange(DOWN, buff = 1)
        food =  VGroup(cooked_turkey, edible_mushroom)
        not_food = VGroup(live_turkey, mario_mushroom)
        four_icons = VGroup(mushrooms, turkeys).arrange(RIGHT, buff=1).shift(3 * LEFT)
        self.play(FadeIn(four_icons), runtime = 4)
        self.wait(.2)
        self.next_slide()

        biologist = SVGMobject(file_name="svgs/biologist.svg", color = GREEN_A, fill_color = GREEN_A)
        hungryman = SVGMobject(file_name="svgs/hungry-man.svg", color = GOLD_A, fill_color = GOLD_A)
        mario = SVGMobject(file_name="svgs/SuperMario.svg")
        perspectives = VGroup(biologist, hungryman, mario).arrange(RIGHT, buff = .75)
        perspectives.shift(3 * RIGHT)
        all = VGroup(four_icons, perspectives)

        rect_around_turkeys = SurroundingRectangle(turkeys, color = GREEN_A)
        turkey_label = Text("Turkeys", color = GREEN_A).scale(.45).next_to(rect_around_turkeys, DOWN)
        rect_around_mushrooms = SurroundingRectangle(mushrooms, color=GREEN_A)
        mushroom_label = Text("Mushrooms", color = GREEN_A).scale(.45).next_to(rect_around_mushrooms, DOWN)
        biologist_grouping = VGroup(rect_around_turkeys, rect_around_mushrooms, turkey_label, mushroom_label)
        self.play(FadeIn(biologist))
        self.play(Write(biologist_grouping))
        self.wait(.2)
        self.next_slide()

        rect_around_food = SurroundingRectangle(food, color=GOLD_A)
        food_label = Text("Food", color=GOLD_A).scale(.45).next_to(rect_around_food, LEFT)
        rect_around_notfood = SurroundingRectangle(not_food, color=GOLD_A)
        not_food_label = Text("Not Food", color=GOLD_A).scale(.45).next_to(rect_around_notfood, LEFT)
        hungry_grouping = VGroup(rect_around_food, food_label, rect_around_notfood, not_food_label)
        self.play(FadeIn(hungryman), FadeOut(biologist_grouping))
        self.play(Write(hungry_grouping))
        self.wait(.2)
        self.next_slide()

        self.play(FadeIn(mario), FadeOut(hungry_grouping))
        self.wait(.2)
        self.next_slide()

        need_context = Text("Grouping requires context!").scale(.75).to_edge(DOWN, buff=1)
        self.play(Write(need_context))
        self.wait(.2)
        self.next_slide()

        leftover = VGroup(need_context, all)
        return title, leftover
    
    def causal_dags(self, title, leftover):
        title = self.change_title("Causal Diagrams", title, leftover)

        # New DAG
        c  = RectVertex("Severity").scale(1.5).shift(2* UP)
        t  = RectVertex("Treatment").scale(1.5).shift(2* LEFT)
        y  = RectVertex("Outcome").scale(1.5).shift(2* RIGHT)

        #season_temp._set_start_and_end_attrs(start=severity.get_edge_center(DOWN) + .2 * LEFT, end = treatment.get_edge_center(UP))
        ct = Line(start=c.get_edge_center(DOWN) + .2 * LEFT, end = t.get_edge_center(UP))
        ct.add_tip(tip_length = .15, tip_width=.15)
        cy = Line(start=c.get_edge_center(DOWN) + .2 * RIGHT, end = y.get_edge_center(UP))
        cy.add_tip(tip_length = .15, tip_width=.15)
        ty = Line(start=t.get_edge_center(RIGHT), end = y.get_edge_center(LEFT), buff=0)
        ty.add_tip(tip_length = .15, tip_width=.15)
        causal_dag = VGroup(c, t, y, cy, ct, ty).next_to(title, DOWN, buff = .7)
        self.play(Write(causal_dag), runtime = 1)
        self.wait(.2)
        self.next_slide()

        # Exogeneous noise
        def noise_node(label, v):
            noise_vertex = RectVertex("Noise", observed=False).next_to(v, UP + 1.2 * RIGHT)
            noise_arrow = DashedLine(start=noise_vertex.get_corner(DL), end = v.get_corner(UR))
            noise_arrow.add_tip(tip_length = .1, tip_width=.1)
            return VGroup(noise_vertex, noise_arrow)
        
        CNoise = noise_node("Condition", c)
        TNoise = noise_node("Treatment", t)
        ONoise = noise_node("Outcome", y)
        self.play(Write(VGroup(CNoise, TNoise, ONoise)))
        self.wait(.2)
        self.next_slide()

        # Generative Model
        def yellow_path(objects_to_yellow):
            self.play(LaggedStart(*[o.animate.set_color(YELLOW) for o in objects_to_yellow], lag_ratio=0.3, run_time=2))
        yellow_path([CNoise[0], CNoise[1], c])
        self.wait(.2)
        self.next_slide()
        yellow_path([TNoise[0], VGroup(TNoise[1], ct), t])
        self.wait(.2)
        self.next_slide()
        yellow_path([ONoise[0], VGroup(ONoise[1], ty, cy), y])
        self.wait(.2)
        self.next_slide()

        # Remove noises and reset color
        self.play(FadeOut(VGroup(CNoise, TNoise, ONoise)), causal_dag.animate.set_color(WHITE))
        self.wait(.2)
        self.next_slide()


        # Draw some people
        group_of_sticks_Treated = StickFigure.group_of_sticks(n=16, width=8, scale=.5, v_sep=1, h_sep=.5, red_prob=.75)
        group_of_sticks_Untreated = StickFigure.group_of_sticks(n=16, width=8, scale=.5, v_sep=1, h_sep=.5, red_prob=.25)
        group_of_sticks_Treated.to_corner(DL, buff = 1)
        group_of_sticks_Untreated.to_corner(DR, buff = 1)
        all_sticks = VGroup(group_of_sticks_Treated, group_of_sticks_Untreated)
        treated_rect = SurroundingRectangle(group_of_sticks_Treated, color=WHITE, buff=0.1, corner_radius=0.1)
        treated_label = Pill().scale(.7).next_to(treated_rect, UP)#Text("Treated", font_size=18).next_to(treated_rect, UP)
        treated_section = VGroup(treated_rect, treated_label)
        untreated_rect = SurroundingRectangle(group_of_sticks_Untreated, color=WHITE, buff=0.1, corner_radius=0.1)
        untreated_label = Pill(treated = False).scale(.5).next_to(untreated_rect, UP)#Text("Untreated", font_size=18).next_to(untreated_rect, UP)
        untreated_section = VGroup(untreated_rect, untreated_label)
        self.play(FadeIn(group_of_sticks_Treated), 
                  FadeIn(group_of_sticks_Untreated), 
                  FadeIn(treated_section), 
                  FadeIn(untreated_section))
        self.wait(.2)
        self.next_slide()

        # Remove arrow and make equal (8 each)
        title = self.change_title("Counterfactuals in Causal Diagrams", title)
        self.wait(.2)
        self.next_slide()
        # make all but 8 of reds in group_of_sticks_Treated blue
        reds = [stick for stick in group_of_sticks_Treated.submobjects if stick.color == RED]
        blues = [stick for stick in group_of_sticks_Untreated.submobjects if stick.color == BLUE]
        self.play(*[turn_blue.animate.set_color(BLUE) for turn_blue in random.sample(reds, len(reds) - 8)],
                     *[turn_red.animate.set_color(RED) for turn_red in random.sample(blues, len(blues) - 8)], Unwrite(ct))
        self.wait(.2)
        self.next_slide()

        def replace_dag_with_new_labels(c_lab, t_lab, y_lab):
            new_c  = RectVertex(c_lab).scale(1.5).move_to(c)
            new_t  = RectVertex(t_lab).scale(1.5).move_to(t)
            new_y  = RectVertex(y_lab).scale(1.5).move_to(y)
            new_ct = Line(start=new_c.get_edge_center(DOWN) + .2 * LEFT, end = new_t.get_edge_center(UP))
            new_ct.add_tip(tip_length = .15, tip_width=.15)
            new_cy = Line(start=new_c.get_edge_center(DOWN) + .2 * RIGHT, end = new_y.get_edge_center(UP))
            new_cy.add_tip(tip_length = .15, tip_width=.15)
            new_ty = Line(start=new_t.get_edge_center(RIGHT), end = new_y.get_edge_center(LEFT), buff=0)
            new_ty.add_tip(tip_length = .15, tip_width=.15)
            return new_c, new_t, new_y, new_ct, new_cy, new_ty, [TransformMatchingShapes(a, b) for (a,b) in [(c, new_c), (t, new_t), (y, new_y), (cy, new_cy), (ty, new_ty)]]

        def replace_figs(group, new_obj, set_color = True, red_prob = .5):
            if set_color:
                objs = [new_obj().move_to(obj).set_color(BLUE) for obj in group]
                for turn_red in random.sample(objs, int(len(objs) * red_prob)):
                    turn_red.set_color(RED) 
                return VGroup(*objs)
            else:
                return VGroup(*[new_obj().move_to(obj) for obj in group])

        pill = lambda: Pill().scale(.3)
        virus = lambda: SVGMobject(file_name = "svgs/virus.svg").scale(.15)
        bacterium = lambda: SVGMobject(file_name = "svgs/bacteria.svg").scale(.15)

        # Replace with Pills
        treated_pills = replace_figs(group_of_sticks_Treated, pill)
        treated_nopills = replace_figs(group_of_sticks_Untreated, pill, set_color=False)
        c, t, y, ct, cy, ty, graph_transform = replace_dag_with_new_labels("Quality", "Treatment", "Effectiveness")
        self.play(FadeOut(group_of_sticks_Treated), FadeOut(group_of_sticks_Untreated))
        self.play(*graph_transform, FadeIn(treated_pills), FadeIn(treated_nopills))
        self.wait(.2)
        self.next_slide()

        # Replace with Viruses
        treated_viruses = replace_figs(group_of_sticks_Treated, virus)
        untreated_viruses = replace_figs(group_of_sticks_Untreated, virus)
        c, t, y, ct, cy, ty, graph_transform = replace_dag_with_new_labels("Virus Strain", "Vaccination", "Effectiveness")
        self.play(FadeOut(treated_pills), FadeOut(treated_nopills))
        self.play(*graph_transform, FadeIn(treated_viruses), FadeIn(untreated_viruses))
        self.wait(.2)
        self.next_slide()

        # Replace with Bacteria
        treated_bact = replace_figs(group_of_sticks_Treated, bacterium, red_prob = .25)
        untreated_bact = replace_figs(group_of_sticks_Untreated, bacterium, red_prob = .75)
        c, t, y, ct, cy, ty, graph_transform = replace_dag_with_new_labels("Resistance", "Antibiotic", "Effectiveness")
        self.play(FadeOut(treated_viruses), FadeOut(untreated_viruses))
        self.play(*graph_transform, FadeIn(treated_bact), FadeIn(untreated_bact), Write(ct))
        self.wait(.2)
        self.next_slide()


        # Unobserved
        new_c = RectVertex("Resistance", observed=False).scale(1.5).move_to(c)
        new_cy = DashedLine(start=new_c.get_edge_center(DOWN) + .2 * RIGHT, end = y.get_edge_center(UP))
        new_cy.add_tip(tip_length = .2, tip_width=.2)
        new_ct = DashedLine(start=new_c.get_edge_center(DOWN) + .2 * LEFT, end = t.get_edge_center(UP))
        new_ct.add_tip(tip_length = .2, tip_width=.2)
        self.play(TransformMatchingShapes(c, new_c), TransformMatchingShapes(cy, new_cy), TransformMatchingShapes(ct, new_ct), VGroup(treated_bact, untreated_bact).animate.set_color(GRAY))
        self.wait(.2)
        self.next_slide()

        # Abstraction
        # Transform into abstracted dag of U, T, Y
        abs_u = Vertex("U", observed = False).shift(2 * UP)
        abs_t = Vertex("T").shift(LEFT)
        abs_y = Vertex("Y").shift(RIGHT)
        ut = Edge(abs_u, abs_t, observed=False)
        uy = Edge(abs_u, abs_y, observed=False)
        ab_ty = Edge(abs_t, abs_y)
        self.play(*[TransformMatchingShapes(a, b) for (a, b) in [(new_c, abs_u), (t, abs_t), (y, abs_y), (new_ct, ut), (new_cy, uy), (ty, ab_ty)]], 
                  FadeOut(VGroup(treated_bact, untreated_bact, treated_section, untreated_section)))
        self.wait(.2)
        self.next_slide()

        # Instrumental Variable
        title = self.change_title("Instrumental Variables", title)
        abs_dag = VGroup(abs_u, abs_t, abs_y, ut, uy, ab_ty)
        self.wait(.2)
        self.next_slide()
        instrument = Vertex("I").shift(3 * LEFT)
        instrument_edge = Edge(instrument, abs_t)
        self.play(Write(instrument), Write(instrument_edge))
        self.wait(.2)
        self.next_slide()

        independence = MathTex("I", r'\perp\!\!\!\!\perp', 'U').next_to(abs_dag, DOWN, buff = .5)
        function_independence = MathTex("f(I)", r'\perp\!\!\!\!\perp', 'U').next_to(abs_dag, DOWN, buff = .5)
        self.play(Write(independence))
        self.wait(.2)
        self.next_slide()
        self.play(TransformMatchingTex(independence, function_independence))
        self.wait(.2)
        self.next_slide()

        proceedure = VGroup(Tex(r"1. Train $\hat{T}(I)$.", color=YELLOW), Tex(r"2. Train $\hat{Y}(\hat{T}(I))$.", color=YELLOW)).arrange(DOWN, buff = .5).next_to(function_independence, DOWN, buff = .5)
        instrument_edge.generate_target()
        instrument_edge.target.set_color(YELLOW)
        self.play(Write(proceedure[0]), MoveToTarget(instrument_edge))
        self.wait(.2)
        self.next_slide()

        instrument_edge.generate_target()
        instrument_edge.target.set_color(WHITE)
        ab_ty.generate_target()
        ab_ty.target.set_color(YELLOW)
        self.play(Write(proceedure[1]), proceedure[0].animate.set_color(WHITE), MoveToTarget(instrument_edge), MoveToTarget(ab_ty))
        self.wait(.2)
        self.next_slide()

        instruments_cite = cite_group("Angrist, J., & Imbens, G. (1995).", "Identification and estimation of local average treatment effects.", "Econometrica. 62 (2): 467–476.")
        instruments_cite.to_edge(RIGHT, buff = .5)
        self.play(FadeIn(instruments_cite))
        self.wait(.2)
        self.next_slide()

        ab_ty.generate_target()
        ab_ty.target.set_color(WHITE)
        self.play(MoveToTarget(ab_ty))

        leftover = VGroup(proceedure, instruments_cite, function_independence)
        abs_dag = VGroup(abs_u, abs_t, abs_y, ut, uy, ab_ty)
        abs_dag.add(instrument)
        abs_dag.add(instrument_edge)
        remember = abs_dag
        return title, leftover, remember

    def introduce_mtes(self, title, leftover):
        title = self.change_title("Mixtures of Causal Relationships", title, everything=leftover)
            
        treatment_hetero = Text("Heterogeneity in Treatment").scale(.5)
        #causal_dag_treat = dag("Drug Quality", "Drug Dosage", "Outcome", dashed_ct = True).next_to(treatment_hetero, DOWN, buff = .25).scale(.5)
        pill = lambda: SVGMobject(file_name = "svgs/pill.svg", color = PURPLE, fill_color = PURPLE).scale(.12)
        pills = VGroup(*[pill() for i in range(25)]).arrange_in_grid(5, 5, buff=.1).next_to(treatment_hetero, DOWN, buff = .5)
        treatment_label = Text("E.g. Quality Control").scale(.3).next_to(pills, DOWN, buff = .5)
        t_total = VGroup(treatment_hetero, pills, treatment_label)
        
        subject_hetero = Text("Heterogeneity in Subject").scale(.5)
        #causal_dag_subject = dag("Strain", "Vaccine", "Effectiveness", dashed_ct = True).next_to(subject_hetero, DOWN, buff = .25).scale(.5)
        virus = lambda: SVGMobject(file_name = "svgs/virus.svg", color = PURPLE, fill_color = PURPLE).scale(.12).set_color(PURPLE)
        viruses = VGroup(*[virus() for i in range(25)]).arrange_in_grid(5, 5, buff=.1).next_to(subject_hetero, DOWN, buff = .5)
        subject_label = Text("E.g. Vaccine Effectiveness").scale(.3).next_to(viruses, DOWN, buff=.5)
        s_total = VGroup(subject_hetero, viruses, subject_label)

        confounding_hetero = Text("Confounding Heterogeneity").scale(.5)
        #causal_dag_confounding = dag("Bacteria", "Antibiotic", "Effectiveness", dashed_ct = False).next_to(confounding_hetero, DOWN, buff = .25).scale(.5)
        bacterium = lambda: SVGMobject(file_name = "svgs/bacteria.svg", color = PURPLE, fill_color = PURPLE).scale(.12)
        bacteria = VGroup(*[bacterium() for i in range(25)]).arrange_in_grid(5, 5, buff=.1).next_to(confounding_hetero, DOWN, buff = .5)
        bacteria_label = Text("E.g. Antibiotic Resistance").scale(.3).next_to(bacteria, DOWN, buff=.5)
        c_total = VGroup(confounding_hetero, bacteria, bacteria_label)

        color_options = [RED, BLUE]
        rand_color = lambda : random.choice(color_options)

        # first dag
        t_total.next_to(title, DOWN, buff=.5)
        self.play(Create(treatment_hetero))
        #self.play(Create(causal_dag_treat))
        self.wait(.2)
        self.next_slide()
        self.play(FadeIn(pills), Write(treatment_label))
        self.wait(.2)
        self.next_slide()
        self.play(*[pill.animate.set_color(rand_color()) for pill in pills])
        self.wait(.2)
        self.next_slide()

        # second dag
        t_total.generate_target()
        running_total = VGroup(t_total.target, s_total).arrange(RIGHT, buff = 1)
        running_total.next_to(title, DOWN, buff=.5)
        self.play(MoveToTarget(t_total), Create(subject_hetero))
        #self.play(Create(causal_dag_subject))
        self.wait(.2)
        self.next_slide()
        self.play(FadeIn(viruses), Write(subject_label))
        self.wait(.2)
        self.next_slide()
        self.play(*[virus.animate.set_color(rand_color()) for virus in viruses])
        self.wait(.2)
        self.next_slide()

        # third dag
        t_total.generate_target()
        s_total.generate_target()
        running_total = VGroup(t_total.target, s_total.target, c_total).arrange(RIGHT, buff = 1)
        running_total.next_to(title, DOWN, buff=.5)
        self.play(MoveToTarget(s_total), MoveToTarget(t_total), Create(confounding_hetero))
        #self.play(Create(causal_dag_confounding))
        self.wait(.2)
        self.next_slide()
        self.play(FadeIn(bacteria), Write(bacteria_label))
        self.wait(.2)
        self.next_slide()
        self.play(*[bacterium.animate.set_color(rand_color()) for bacterium in bacteria])
        self.wait(.2)
        self.next_slide()

        #confounders = VGroup(*[VGroup(dag[0], dag[3], dag[5]) for dag in [causal_dag_confounding, causal_dag_subject, causal_dag_treat]])
        #confounders.generate_target()
        #confounders.target.set_opacity(.4)
        #things_to_grey = VGroup(viruses, bacteria, pills)
        #self.play(MoveToTarget(confounders), things_to_grey.animate.set_color(GRAY))
        MTEs = Text("Mixtures of Treatment Effects (MTEs)").scale(.7)
        Challenge1 = Text("Challenge #1: Confounded Causal Relationships").scale(.5)
        Challenge2 = Text("Challenge #2: Overlapping Covariate Support").scale(.5)
        mtes_and_challenges = VGroup(MTEs, Challenge1, Challenge2).arrange(DOWN, buff = .5).to_edge(DOWN, buff=.7)
        for text in mtes_and_challenges:
            self.play(Create(text))
            self.wait(.2)
            self.next_slide()
        self.play(Challenge1.animate.set_color(YELLOW))
        self.wait(.2)
        self.next_slide()
        leftover = VGroup(s_total, t_total, c_total, mtes_and_challenges)
        return title, leftover
    
    def mixtures_of_relationships(self, title, leftover):
        title = self.change_title("(Graphical) Mixture Models", title, leftover)

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
        title = self.change_title("Clustering", title, observed)
        self.play(FullGraph.animate.align_on_border(LEFT))
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
        
        ###### We have this problem
        self.play(right_half.animate.shift(10 * RIGHT), left_half.animate.shift(10 * LEFT))
        title = self.change_title("Mixtures of Relationships", title, VGroup(left_half, right_half))
        xaxes = Axes(
            x_range=[-2, 2, .5],
            y_range=[-2, 2, .5],
            x_length=5,
            y_length=5,
            axis_config={"color": WHITE, "include_numbers": False},
            tips=False,
        ).next_to(title, DOWN, buff=1)

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
        leftover = x_graph

        ########## Method of Moments
        title = self.change_title("Algebraic Approach", title, leftover)
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
        moments_cite = cite_group('Pearson, K. (1936). Method of Moments and Method of Maximum Likelihood. Biometrika 28(1/2), 35–59.')
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
        leftover = VGroup(MoM_full, inclusion_diagram)      
        return title, leftover
    
    def confounding(self, title, leftover):
        title = self.change_title("Fundimental Problem of Causal Inference", title, leftover)
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
            cite_group('Neyman, Jerzy (1923).','Sur les applications de la theorie des probabilites aux experiences agricoles: Essai des principes.','Roczniki Nauk Rolniczych, 10(1), 1-51.').scale(.8),
            cite_group('Rubin, Donald (1974).','Estimating Causal Effects of Treatments in Randomized and Nonrandomized Studies.','J. Educ. Psychol. 66 (5): 688–701 [p. 689].').scale(.8)
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
        bot = VGroup(po0, untreatment_arrow, person_untreated, te.submobjects[6])
        bot.generate_target()
        bot.target.set_opacity(.3)
        self.play(MoveToTarget(bot), run_time = .5)
        self.wait(.2)
        self.next_slide()

        top = VGroup(po1, treatment_arrow, person_treated, te[3])
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
        CovAdjustments_cite = cite_group('Pearl, J. (2009). Causality. Cambridge university press.')
        full_cite = VGroup(CovAdjustments, CovAdjustments_cite).arrange(DOWN, center=True).to_corner(DR, buff = .5)
        self.play(*[stick.animate.set_color(c) for stick, c in zip(group_of_sticks, treated_red_blues)],
                    *[stick.animate.set_color(c) for stick, c in zip(group_of_sticks_treated, treated_red_blues)],
                    *[stick.animate.set_color(c) for stick, c in zip(group_of_sticks2, untreated_red_blues)],
                    *[stick.animate.set_color(c) for stick, c in zip(group_of_sticks_untreated, untreated_red_blues)],
                    FadeIn(full_cite))
        self.wait(.2)
        self.next_slide()

        leftover = VGroup(full_cite, ate, apo1, apo0, group_of_sticks, group_of_sticks2, group_of_sticks_treated, group_of_sticks_untreated, treated, untreated, treatment_arrow_2, untreatment_arrow_2)
        return title, leftover
    
    def tensor_methods(self, title, leftover):
        title = self.change_title("Tensor Methods", title, leftover)

        V1 = Vertex(label = r'V_1').shift(3 * LEFT)
        V2 = Vertex(label = r'V_2')
        V3 = Vertex(label = r'V_3').shift(3 * RIGHT)
        V = VGroup(V1, V2, V3)
        U = Vertex(label=r'U', observed=False).shift(2 * UP)

        U_Edges = VGroup(*[Edge(U, V, observed = False) for V in V])
        FullGraph = VGroup(V, U, U_Edges)
        self.play(Create(FullGraph))
        self.wait(.2)
        self.next_slide()

        params = Tex(r"Parameters: $\mathbb{P}(u^{(i)})$ and $\mathbb{P}( v_j \; | \; u^{(i)})$ for $i \in \{0,\ldots k-1\}, j \in \{1,2,3\}$").scale(.5)
        moments = Tex(r"Obervable Moments: $\mathbb{E}[v_1, v_2, v_3]$ with $v_j \in \{1, \ldots, n_j\}$").scale(.5)
        Thm = Tex(r"Generic identifiability when $\min(n_1, k) + \min(n_2, k) + \min(n_3, k) \geq 2k + 2$").scale(.5)
        citation1 = cite_group('J. Kruskal (1977).','Three-way arrays: Rank and uniqueness of trilinear decompositions, with application to arithmetic complexity and statistics.','Linear algebra and its applications, 18(2), 95-138.').scale(.8)
        citation2 = cite_group('E. Allman, C. Matias, J. Rhodes (2009).','Identifiability of parameters in latent structure models with many observed variables.','Ann. Statist. 37(6A): 3099-3132.').scale(.8)
        citation3 = cite_group('A. Anandkumar, R. Ge, D. Hsu, S. Kakade, M. Telgarsky (2014).','Tensor decompositions for learning latent variable models".','J. Mach. Learn. Res., 15(1), 2773-2832.').scale(.8)
        ident_result = VGroup(params, moments, Thm, citation1, citation2, citation3)
        ident_result.arrange(DOWN, center=True, buff = .15).next_to(FullGraph, DOWN, buff = .5)
        for t in ident_result:
            self.play(FadeIn(t))
            self.wait(.2)
            self.next_slide()

        everything = VGroup(ident_result)
        title = self.change_title("Tensor Methods for MTEs", title, everything)
        T, Y, UT, UY= self.duplicate(V2, U_Edges[1], U, ["T", "Y"])
        TY = Edge(T, Y)
        self.play(Create(TY))
        self.wait(.2)
        self.next_slide()

        CATEs = Tex(r"MTEs: $\mathbb{E}[Y^{(1)} - Y^{(0)} \; | \; u] = \mathbb{E}[Y \; | \; T=1, u] - \mathbb{E}[Y \; | \; T=0, u]$").scale(.75)
        ClearCite = cite_group('S.L. Gordon, B. Mazaheri, Y. Rabani, and L. Schulman (2023).','Causal Inference Despite Limited Global Confounding via Mixture Models.','In Conference on Causal Learning and Reasoning (pp. 574-601). PMLR.').scale(1.2)
        DoesntMatter1 = Tex(r"$\mathbb{E}[V_1 \;|\; u], \mathbb{E}[V_3 \;|\; u]$ are useless parameters!").scale(.5)
        DoesntMatter2 = Tex(r"Only the \emph{difference} between potential outcomes matters!").scale(.5)
        CATEs_and_DMs = VGroup(CATEs, ClearCite, DoesntMatter1, DoesntMatter2).arrange(DOWN, buff=.4).next_to(FullGraph, DOWN, buff=.5)
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
        leftover = VGroup(CATEs_and_DMs, T, Y, U, V1, V3, TY, UT, UY, U_Edges[0], U_Edges[2])
        #self.play(FadeOut(leftover))
        #self.wait(.2)
        return title, leftover
    
    def spos_ate(self, title, leftover, remember):
        U, T, Y, UT, UY, TY, I, ZT = remember.submobjects
        title = self.change_title("Negative Controls", title, leftover)
        self.wait(.2)
        self.next_slide()
        Z = Vertex(label = "Z").move_to(I)
        X = Vertex(label = "X").shift(3 * RIGHT)
        XY = Edge(X, Y)
        UX = Edge(U, X, observed=False)
        UZ = Edge(U, Z, observed=False)
        TZ = Edge(T, Z)
        self.play(TransformMatchingShapes(I, Z), Create(X), Write(XY), Write(ZT), Write(UX), Write(UZ))
        FullGraph = VGroup(U, T, Y, X, Z, UT, UY, UX, UZ, TY, ZT, XY)
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

        #This setup is called negative controls
        cite = cite_group('W. Miao, Z. Geng, and E.J. Tchetgen Tchetgen (2018).','Identifying causal effects with proxy variables of an unmeasured confounder.','Biometrika, 105(4), 987-993.')
        cite.next_to(FullGraph, DOWN, buff = 1)
        self.play(FadeIn(cite))
        self.wait(.2)
        self.next_slide()
        self.play(FadeOut(cite))
        self.wait(.2)
        self.next_slide()

        title = self.change_title("Synthetic Potential Outcomes", title)
        self.wait(.2)
        self.next_slide()
        
        #Add vectors
        # Vectorize
        U_vec = MobjectMatrix([[MathTex(r"\mathbb{P}(u^{(0)})").scale(.3), MathTex(r"\mathbb{P}(u^{(1)})").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, h_buff=0.7).next_to(U, UP)
        
        col_vecs = VGroup(*[P_vec(v) for v in [Z, Y, X]])
        self.play(FadeIn(U_vec), FadeIn(col_vecs))
        self.wait(.2)
        self.next_slide()

        ATE_label = Text("ATE:").scale(.75)
        end_wanted_eq = MathTex(r"\mathbb{E}(",r"Y^{(1)}",r" - ", r"Y^{(0)}", r")").scale(.75)
        end_wanted_eq_distributed = MathTex(r"\mathbb{E}(", r"Y^{(1)}", r")", r" - ", r"\mathbb{E}(", r"Y^{(0)}", r")").scale(.75)
        ate_eq_goal = VGroup(ATE_label, end_wanted_eq).arrange(RIGHT, buff = .5).next_to(FullGraph, DOWN, buff = 1.5)
        self.play(FadeIn(ate_eq_goal))
        self.wait(.2)
        self.next_slide()
        end_wanted_eq_distributed.move_to(end_wanted_eq)
        self.play(TransformMatchingShapes(end_wanted_eq, end_wanted_eq_distributed))
        self.wait(.2)
        self.next_slide()

        Wanted_label = Text("Wanted:").set_color(GREEN).scale(.7)
        wanted_eq = VGroup(
            MathTex(r"\mathbb{E}(Y^{(t)}) = ").scale(.75),
            U_vec.copy().scale(1.2),
            E_vec("Y", cond = " t,").scale(1.2)
        ).arrange(RIGHT).set_color(GREEN)
        wanted = VGroup(Wanted_label, wanted_eq).arrange(DOWN, buff = .5).next_to(FullGraph, DOWN, buff = 1.5)

        self.play(TransformMatchingShapes(ATE_label, Wanted_label), TransformMatchingShapes(end_wanted_eq_distributed, wanted))
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
        exp = VGroup(exp_label, exp_eq).arrange(DOWN, buff = .5).next_to(FullGraph, DOWN, buff = 1.5)
        self.play(Create(exp))

        # Turn green
        self.wait(.2)
        self.next_slide()
        self.play(exp_eq[1].animate.set_color(GREEN), U.animate.set_color(GREEN), U_vec.animate.set_color(GREEN))

        # Condition
        self.next_slide()
        U_vec2 = MobjectMatrix([[MathTex(r"\mathbb{P}(u^{(0)} | t)").scale(.3), MathTex(r"\mathbb{P}(u^{(1)}|t)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, h_buff=0.7).move_to(U_vec)
        Y_vec2 = E_vec("Y", cond = " t,").set_color(GREEN).move_to(col_vecs[1])
        Z_vec2 = E_vec("Z", cond = " t,").move_to(col_vecs[0])

        new_exp_label = Text("Conditional Expectation:").scale(.7)
        new_exp_eq = VGroup(
            MathTex(r"\mathbb{E}(Y | t) = ").scale(.75),
            U_vec2.copy().scale(1.2),
            E_vec("Y", cond = " t, ").scale(1.2).set_color(GREEN)
        ).arrange(RIGHT)
        new_exp = VGroup(new_exp_label, new_exp_eq).arrange(DOWN, buff = .5).next_to(FullGraph, DOWN, buff = 1.5)
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
        self.wait(.2)
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
        self.play(TransformMatchingShapes(col_vecs[2], X1_vec),
                  FadeIn(X2_vec))
        
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
        moment_matching_1 = MathTex(r"\mathbb{E}(Y |t) = \alpha_1 \mathbb{E}(X_1|t) + \alpha_2 \mathbb{E}(X_2|t)").scale(.8)
        moment_matching_2 = MathTex(r"\mathbb{E}(Y Z|t) = \alpha_1 \mathbb{E}(X_1 Z|t) + \alpha_2 \mathbb{E}(X_2 Z|t)").scale(.8)
        moment_matching = VGroup(moment_matching_1, moment_matching_2).arrange(DOWN, center=True)
        alpha_derive = VGroup(want_alphas, moment_matching).arrange(DOWN, center=True).next_to(col_vecs, DOWN)
        self.play(Write(want_alpha_eq_structure), MoveToTarget(copyY), MoveToTarget(copyX1), MoveToTarget(copyX2))
        self.wait(.2)
        self.next_slide()

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
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1|t)").scale(.3), MathTex(r"\mathbb{E}(X_2|t)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z|t)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z|t)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y|t)").scale(.3)], [MathTex(r"\mathbb{E}(Y Z|t)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1)
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
                  condition2.animate.set_color(WHITE))
        self.wait(.2)
        self.next_slide()

        # Turn blue and yellow into green
        turn_into_green = Y_vec2.copy().move_to(X1_vec).shift(.5 * RIGHT)
        self.play(TransformMatchingShapes(VGroup(X1_vec, X2_vec), turn_into_green))
        self.wait(.2)
        self.next_slide()

        # Uncondition
        self.play(T.uncondition(), 
                  TransformMatchingShapes(U_vec2, U_vec), 
                  TransformMatchingShapes(Y_vec2, col_vecs[1]),
                  TransformMatchingShapes(Z_vec2, col_vecs[0]),
                  Y.animate.set_color(WHITE))
        self.wait(.2)
        self.next_slide()

        self.play(TransformMatchingShapes(wanted_eq.copy(), new_wanted))
        self.wait(.2)
        self.next_slide()

        moment_matching_matrix_a = VGroup(
            MobjectMatrix([[MathTex(r"\alpha_1").scale(.3)], [MathTex(r"\alpha_2").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1|T=1)").scale(.3), MathTex(r"\mathbb{E}(X_2|T=1)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z|T=1)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z|T=1)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y|T=1)").scale(.3)], [MathTex(r"\mathbb{E}(Y Z|T=1)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4)
        ).arrange(RIGHT)
        moment_matching_matrix_b = VGroup(
            MobjectMatrix([[MathTex(r"\beta_1").scale(.3)], [MathTex(r"\beta_2").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1|T=0)").scale(.3), MathTex(r"\mathbb{E}(X_2 |T=0)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z|T=0)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z|T=0)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y |T=0)").scale(.3)], [MathTex(r"\mathbb{E}(Y Z|T=0)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4)
        ).arrange(RIGHT)
        ATE_eq = VGroup(
            MathTex(r"\mathbb{E}(Y^{(1)}) - \mathbb{E}(Y^{(0)})"),
            MathTex(r"= (\alpha_1 - \beta_1)"),
            MathTex(r"\mathbb{E}(X_1)", color=YELLOW),
            MathTex(r"+ (\alpha_2 - \beta_2)"),
            MathTex(r"\mathbb{E}(X_2)", color = BLUE),
        ).arrange(RIGHT)
        final_calc = VGroup(moment_matching_matrix_a, moment_matching_matrix_b, ATE_eq).arrange(DOWN, center=True).center()
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
        identifiability = Tex(r"Identifiability $\rightarrow$ Matrix Singularity", color=WHITE).scale(.7)
        stability = Tex(r"Stability $\rightarrow$ Matrix Condition Number", color=WHITE).scale(.7)
        cite_gordon = cite_group('S. Gordon, B. Mazaheri, Y. Rabani, and L. Schulman (2021).', 'Source Identification for Mixtures of Product Distributions.', 'In Conference on Learning Theory, pages 2193-2216. PMLR.' )
        main_points = VGroup(identifiability, stability, cite_gordon)
        main_points.arrange(DOWN)
        proceedure.generate_target()
        proceedure.target.next_to(title, DOWN, buff = .5)
        main_points.to_edge(DOWN, buff = .5)
        self.play(MoveToTarget(proceedure))
        for t in main_points:
            self.play(Write(t))
            self.wait(.2)
            self.next_slide()

        self.play(FadeOut(main_points), FadeOut(proceedure))
        self.wait(.2)
        self.next_slide()

        # Reminder / Orientation
        # Fade out all except core graph
        self.play(FadeOut(VGroup(X, UX, Z, UZ, ZT, XY)), FadeOut(title), FadeOut(VGroup(col_vecs[0], col_vecs[1], U_vec, cond_box, wanted, turn_into_green)))
        MTEs = Text("Mixtures of Treatment Effects (MTEs)").scale(.7)
        Challenge1 = Text("Challenge #1: Confounded Causal Relationships").scale(.5)
        Challenge2 = Text("Challenge #2: Overlapping Covariate Support").scale(.5)
        mtes_and_challenges = VGroup(MTEs, Challenge1, Challenge2).arrange(DOWN, buff = .5).to_edge(DOWN, buff=.7)
        self.play(FadeIn(mtes_and_challenges))
        self.wait(.2)
        self.next_slide()
        UT.generate_target()
        UT.target.set_color(YELLOW)
        self.play(Challenge1.animate.set_color(YELLOW), MoveToTarget(UT))
        self.wait(.2)
        self.next_slide()
        UsingSPOs = Text("Solved using SPOs!", color=YELLOW).scale(.5).next_to(Challenge1, RIGHT, buff=1)
        arrow_between_text = Arrow(start = UsingSPOs.get_left(), end=Challenge1.get_right(), color=YELLOW)
        self.play(FadeOut(UT), FadeIn(UsingSPOs), FadeIn(arrow_between_text))
        self.wait(.2)
        self.next_slide()
        new_challenge_1 = VGroup(Challenge1, arrow_between_text, UsingSPOs)
        UY.generate_target()
        UY.target.set_color(YELLOW)
        self.play(MoveToTarget(UY), 
                    new_challenge_1.animate.set_color(WHITE), 
                    Challenge2.animate.set_color(YELLOW))
        self.wait(.2)
        self.next_slide()        

        #remember = VGroup(FullGraph, U_vec, col_vecs, turn_into_green, U_vec2, Z_vec2, X1_vec, X2_vec,cond_box)
        leftover = VGroup(new_challenge_1, MTEs, U, T, Y, UY, TY)
        title = Challenge2
        return title, leftover
    
    def coins(self, title, leftover):
        title = self.change_title("The Power of Higher-Order Moments", title, leftover)
        self.wait(.2)
        self.next_slide()

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
        title = self.change_title("The k-Coin Problem", title, everything=everything)

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
            E_vec("R")
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
        citation = cite_group('J. Li, Rabani, L. Schulman, C. Swamy (2015).','Learning Arbitrary Statistical Mixtures of Discrete Distributions.','In Proceedings of the forty-seventh annual', 'ACM Aymposium on Theory of Computing (pp. 743-752).')
        ident_result = VGroup(dof1, dof2, dof3, identification, citation).shift(.5 * DOWN)
        ident_result.arrange(DOWN, center=True).to_edge(RIGHT, buff = .5)
        for t in ident_result:
            self.play(Write(t))
            self.wait(.2)
            self.next_slide()
        leftover = VGroup(everything, ident_result, U_Edges)
        return title, leftover
    
    def spos_mte(self, title, leftover):
        title = self.change_title("Synthetic Potential Outcomes for MTEs", title, leftover)
        # Get back to where we were
        Z = Vertex("Z").shift(3 * LEFT)
        T = Vertex("T").shift(LEFT)
        Y = Vertex("Y").shift(RIGHT)
        X = Vertex("X").shift(3 * RIGHT)
        U = Vertex("U", observed=False).shift(2 * UP)
        XY = Edge(X, Y)
        TY = Edge(T, Y)
        ZT = Edge(Z, T)
        TZ = Edge (T, Z)
        U_Edges = VGroup(*[Edge(U, V, observed=False) for V in [Z, T, Y, X]])
        FullGraph = VGroup(Z, T, Y, X, U, U_Edges, TY, XY, TZ, ZT)
        self.play(Write(FullGraph))
        self.wait(.2)
        self.next_slide()

        #Write conditions
        conds = Text("Conditions").scale(.7).next_to(FullGraph, DOWN, buff = 1)
        condition1 = MathTex(r"X \perp \!\!\! \perp T \;|\; U")
        condition2 = MathTex(r"Y", r"\perp \!\!\! \perp", r"Z", r"\;|\;", r"T", r"U")
        conditions = VGroup(condition1, condition2).arrange(DOWN, buff = .1).next_to(conds, DOWN)
        cond_box = VGroup(conds, conditions)
        self.play(Create(cond_box))
        self.wait(.2)
        self.next_slide()

        new_condition_2 = MathTex(r"Y", r"\perp \!\!\! \perp", r"Z", r",X", r"\;|\;", r"T", r"U").move_to(condition2)
        self.play(Unwrite(XY), TransformMatchingTex(condition2, new_condition_2))
        conditions = VGroup(condition1, new_condition_2)
        cond_box = VGroup(conds, conditions)
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
        
        col_vecs = VGroup(*[P_vec(v) for v in [Z, Y, X]])
        # Add X1, X2, Z1, Z2 columns
        X1_vec = E_vec(r"X_1").move_to(col_vecs[2])
        X2_vec = E_vec(r"X_2").next_to(X1_vec, RIGHT, buff=1)
        self.play(FadeIn(U_vec), FadeIn(VGroup(col_vecs[0], col_vecs[1], X1_vec, X2_vec)))
        self.wait(.2)
        self.next_slide()

        ATE_label = Text("Wanted:").scale(.75)
        end_wanted_eq = MathTex(r"\mathbb{E}(",r"R^{\odot 2}", r")").scale(.75)
        end_wanted_eq_distributed = MathTex(r"\mathbb{E}(",r"R", r"\odot", r"Y^{(1)}", r")", r"-", r"\mathbb{E}(",r"R", r"\odot", r"Y^{(0)}", r")").scale(.75)
        ate_eq_goal = VGroup(ATE_label, end_wanted_eq).arrange(RIGHT, buff = .5).next_to(FullGraph, DOWN, buff = 1.5)
        self.play(FadeIn(ate_eq_goal))
        self.wait(.2)
        self.next_slide()
        end_wanted_eq_distributed.move_to(end_wanted_eq).shift(1.5 * RIGHT)
        self.play(TransformMatchingShapes(end_wanted_eq, end_wanted_eq_distributed))
        self.wait(.2)
        self.next_slide()

        Wanted_label = Text("Wanted:").scale(.7)
        wanted_eq = VGroup(
            MathTex(r"\mathbb{E}(R \odot Y^{(t)}) = ").scale(.75),
            U_vec.copy().scale(1.2),
            E_vec("R", cond = " ").scale(1.2).set_color(PURPLE),
            MathTex(r"\odot").scale(.75),
            E_vec("Y", cond = " t,").scale(1.2).set_color(GREEN)
        ).arrange(RIGHT)
        wanted = VGroup(Wanted_label, wanted_eq).arrange(DOWN, buff = .5).next_to(FullGraph, DOWN, buff = 1.5)

        self.play(TransformMatchingShapes(ATE_label, Wanted_label), TransformMatchingShapes(end_wanted_eq_distributed, wanted))
        self.wait(.2)
        self.next_slide()
        wanted.generate_target()
        wanted.target.scale(.7).to_corner(UR, buff = .5).shift(DOWN)
        self.play(MoveToTarget(wanted))
        self.wait(.2)
        self.next_slide()

        # Condition
        self.wait(.2)
        self.next_slide()
        U_vec2 = MobjectMatrix([[MathTex(r"\mathbb{P}(u^{(0)} | t)").scale(.3), MathTex(r"\mathbb{P}(u^{(1)}|t)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, h_buff=0.7).move_to(U_vec)
        Y_vec2 = E_vec("Y", cond = " t,").set_color(GREEN).move_to(col_vecs[1])
        Z_vec2 = E_vec("Z", cond = " t,").move_to(col_vecs[0])

        self.play(T.condition(), 
                  TransformMatchingShapes(U_vec, U_vec2), 
                  TransformMatchingShapes(col_vecs[1], Y_vec2),
                  TransformMatchingShapes(col_vecs[0], Z_vec2),
                  U.animate.set_color(WHITE),
                  Y.animate.set_color(GREEN))
        self.wait(.2)
        self.next_slide()
        
        # Remember that we can combine Xs to get purple
        turn_to_purple = E_vec("R", cond = " ").set_color(PURPLE).move_to(X1_vec).shift(.5 * RIGHT)
        self.play(TransformMatchingShapes(VGroup(X1_vec, X2_vec), turn_to_purple))
        self.wait(.2)
        self.next_slide()

        self.play(new_condition_2.animate.set_color(YELLOW))
        self.wait(.2)
        self.next_slide()

        colored_vecs_copy = VGroup(turn_to_purple, Y_vec2.copy())
        exp_eq = VGroup(
            MathTex(r"\mathbb{E}(R \odot Y | t) = ").scale(.7),
            MathTex(r"\gamma_1 \mathbb{E}(X_1 \odot Y | t) + \gamma_2\mathbb{E}(X_2 \odot Y | t) = ").scale(.7),
            U_vec2.copy().scale(1),
            E_vec("Y", cond = " t, ").scale(1).set_color(GREEN),
            MathTex(r"\odot").scale(.75),
            E_vec("R", cond = " ").scale(1).set_color(PURPLE)
        ).arrange(RIGHT)
        exp_eq.next_to(FullGraph, DOWN, buff = 2)
        self.play(TransformMatchingShapes(colored_vecs_copy, exp_eq))
        self.wait(.2)
        self.next_slide()
        # have X1 and X2 reappear, then rearrange into the product
        goal_vec = VGroup(
            E_vec("Y", cond = " t, ").scale(1).set_color(GREEN),
            MathTex(r"\odot").scale(.75),
            E_vec("R", cond = " ").scale(1).set_color(PURPLE)
        ).arrange(RIGHT).move_to(VGroup(X1_vec, X2_vec))
        self.play(FadeIn(X1_vec), FadeIn(X2_vec))
        self.play(TransformMatchingShapes(VGroup(X1_vec, X2_vec), goal_vec))
        self.wait(.2)
        self.next_slide()

        self.play(FadeOut(exp_eq), new_condition_2.animate.set_color(WHITE))
        self.wait(.2)
        self.next_slide()
        

        param_vec = lambda p, n: MobjectMatrix([[MathTex(p + r"^{(" + str(n) + r")}_1").scale(.3)], [MathTex(p + r"^{(" + str(n) + r")}_2").scale(.3)]],
                                                bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4)
        moment_matching_matrix_a = VGroup(
            param_vec(r"\alpha", 2),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1 |T=1)").scale(.3), MathTex(r"\mathbb{E}(X_2|T=1)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z|T=1)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z|T=1)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y X_1 |T=1)").scale(.3), MathTex(r"\mathbb{E}(Y X_2|T=1)").scale(.3)], [MathTex(r"\mathbb{E}(Y X_1 Z|T=1)").scale(.3), MathTex(r"\mathbb{E}(Y X_2 Z|T=1)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            param_vec(r"\gamma", 1)
        ).arrange(RIGHT)
        moment_matching_matrix_b = VGroup(
            param_vec(r"\beta", 2),
            MathTex("="),
            MobjectMatrix([[MathTex(r"\mathbb{E}(X_1|T=0)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z_1|T=0)").scale(.3)], [MathTex(r"\mathbb{E}(X_1 Z|T=0)").scale(.3), MathTex(r"\mathbb{E}(X_2 Z|T=0)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            MobjectMatrix([[MathTex(r"\mathbb{E}(Y X_1|T=0)").scale(.3), MathTex(r"\mathbb{E}(Y X_2 Z_1|T=0)").scale(.3)], [MathTex(r"\mathbb{E}(Y X_1 Z|T=0)").scale(.3), MathTex(r"\mathbb{E}(Y X_2 Z|T=0)").scale(.3)]], bracket_h_buff=0.05, bracket_v_buff=0.05, v_buff=0.5, h_buff = 1.4),
            param_vec(r"\gamma", 1)
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
        final_calc = VGroup(moment_matching_matrix_a, moment_matching_matrix_b, gamma_def, ATE_eq).arrange(DOWN, center=True).center()
        inverse_a = MathTex(r"-1").scale(.3).next_to(moment_matching_matrix_a[2], RIGHT, buff=0.05).shift(.3 * UP)
        inverse_b = MathTex(r"-1").scale(.3).next_to(moment_matching_matrix_b[2], RIGHT, buff=0.05).shift(.3 * UP)
        main_highlight = BackgroundRectangle(final_calc, color = WHITE, stroke_width=8, stroke_opacity=1, fill_opacity=1, fill_color = BLACK, buff=.3)
        self.play(FadeIn(main_highlight),
                  FadeIn(final_calc),
                  FadeIn(inverse_a),
                  FadeIn(inverse_b))
        self.wait(.2)
        self.next_slide()
        FullGraph = VGroup(Z, T, Y, X, U, U_Edges, TY, TZ, ZT)
        leftover = VGroup(goal_vec, final_calc, main_highlight, inverse_a, inverse_b, FullGraph, X1_vec, X2_vec, cond_box, wanted, U_vec2, turn_to_purple, Z_vec2, Y_vec2)
        self.play(T.uncondition())
        return title, leftover
    
    def takeaways(self, title, leftover):
        title = self.change_title("Hierarchy of Identifiability", title, leftover)
        self.wait(.2)
        self.next_slide()

        Z, T, Y, X = Vertex("Z"), Vertex("T"), Vertex("Y"), Vertex("X")
        Vertices = VGroup(Z, T, Y, X).arrange(RIGHT, buff = .5)
        U = Vertex("U", observed=False).next_to(Vertices, UP, buff=.3)
        Edges = VGroup(Edge(T,Y), Edge(Z,T), Edge(X,Y), Edge(T, Z))
        U_Edges = VGroup(*[Edge(U, V, observed=False) for V in Vertices]) 
        ATE_DAG = VGroup(Vertices, U, Edges, U_Edges)
        
        Z, T, Y, X = Vertex("Z"), Vertex("T"), Vertex("Y"), Vertex("X")
        Vertices = VGroup(Z, T, Y, X).arrange(RIGHT, buff = .5)
        U = Vertex("U", observed=False).next_to(Vertices, UP, buff=.3)
        Edges = VGroup(Edge(T,Y), Edge(Z,T), Edge(T, Z))
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
        conc4 = Text("4. Causal modeling is very useful!")
        
        conclusions = VGroup(conc1.scale(.38), conc2.scale(.38), conc3.scale(.38), conc4.scale(.5)).arrange(DOWN, aligned_edge = LEFT, buff = .5).to_edge(RIGHT, buff = .5)
        for conc in conclusions:
            self.wait(.2)
            self.next_slide()
            self.play(Create(conc))

        citation = cite_group('B. Mazaheri, C. Squires and C. Uhler (2024).','Synthetic Potential Outcomes and Causal Mixture Identifiability.', 'Preprint on arXiv!')
        citation.to_corner(DR, buff=.5)
        self.wait(.2)
        self.next_slide()
        self.play(Create(citation))
        self.wait(.2)
        self.next_slide()

        leftover = VGroup(conclusions, full_stack, citation)
        return title, leftover
    
    def outlook(self, title, leftover):
        title = self.change_title("Outlook: Signal Carving", title, leftover)
         # Draw DAG
        def dag_generator(clab, xlab, ylab):
            c  = RectVertex(clab).shift(1.5 * UP)
            x  = RectVertex(xlab).shift(1.5 * LEFT)
            y  = RectVertex(ylab).shift(1.5 * RIGHT)

            cx = Line(start=c.get_edge_center(DOWN) + .2 * LEFT, end = x.get_edge_center(UP), buff=0)
            cx.add_tip(tip_length = .1, tip_width=.1)
            cy = Line(start=c.get_edge_center(DOWN) + .2 * RIGHT, end = y.get_edge_center(UP), buff=0)
            cy.add_tip(tip_length = .1, tip_width=.1)
            xy = Line(start=x.get_edge_center(RIGHT), end = y.get_edge_center(LEFT), buff=0)
            xy.add_tip(tip_length = .1, tip_width=.1)

            cd = VGroup(c, x, y, cx, cy, xy)
            cd.scale(1.5).move_to(ORIGIN)
            return cd, cx, cy, xy

        causal_dag, cx, cy, xy = dag_generator("Batch", "Drug", "scRNA")
        self.play(Write(causal_dag), run_time = 1)
        self.wait(.2)
        self.next_slide()

        # Remove both
        self.play(*fade_to_gray([cx, cy]), runtime = 1)
        self.wait(.2)
        self.next_slide()

        causal_dag.generate_target()
        causal_dag.target.to_edge(LEFT, buff=1)
        self.play(MoveToTarget(causal_dag))

        ten_batches = VGroup(*[Dot(radius = .15, color=random_bright_color()) for i in range(10)]).arrange(DOWN, buff = .25)
        ten_drugs = VGroup(*[Dot(radius = .15, color=random_bright_color()) for i in range(10)]).arrange(DOWN, buff = .25)
        diag = VGroup(ten_batches, ten_drugs).arrange(RIGHT, buff = 2)
        drug_to_batches = dict()
        for drug in ten_drugs:
            drug_to_batches[drug] = [[], []]
        lines = []
        for batch in ten_batches:
            for i in np.random.choice(10, 4):
                lines.append(Line(batch, ten_drugs[i]))
                drug_to_batches[ten_drugs[i]][0].append(batch)
                drug_to_batches[ten_drugs[i]][1].append(lines[-1])
        connections = VGroup(*lines)
        batches_label = Text("Batches").scale(.5).next_to(ten_batches, LEFT)
        drugs_label = Text("Drugs").scale(.5).next_to(ten_drugs, RIGHT)
        diagram = VGroup(ten_batches, ten_drugs, connections, batches_label, drugs_label).to_edge(RIGHT, buff = .5)
        self.play(Write(diagram))
        self.wait(.2)
        self.next_slide()

        # fade all but drug 1 and its batches
        selected_drug = ten_drugs[4]
        for drug in ten_drugs:
            drug.generate_target()
            if drug != selected_drug:
                drug.target.set_opacity(.3)
        for batch in ten_batches:
            batch.generate_target()
            if batch not in drug_to_batches[selected_drug][0]:
                batch.target.set_opacity(.3)
        for con in connections:
            con.generate_target()
            if con not in drug_to_batches[selected_drug][1]:
                con.target.set_opacity(.3)
        self.play(*[MoveToTarget(d) for d in ten_drugs], *[MoveToTarget(b) for b in ten_batches], *[MoveToTarget(c) for c in connections])
        self.wait(.2)
        self.next_slide()

        self.play(*fade_to_gray([xy]))
        self.wait(.2)
        self.next_slide()

        self.play(FadeOut(diagram))
        causal_dag.generate_target()
        causal_dag.target.center()
        self.play(MoveToTarget(causal_dag))

        # Remove Incoming X
        self.play(*fade_back([cx, cy, xy]), runtime = 1)
        self.wait(.2)
        self.next_slide()
        self.play(*fade_to_gray([cx]))
        self.wait(.2)
        self.next_slide()
        
        self.play(*fade_back([cx]))
        indep = Tex("Outcome " + r"$\perp \!\!\!\! \perp$" +  " Batch " + r"$\; | \;$" + " Drug").next_to(causal_dag, DOWN)
        self.play(Write(indep))
        self.play(*fade_to_gray([cy]), runtime = 1)
        self.wait(.2)
        self.next_slide()

        def dag_and_eq(clab, xlab, ylab):
            causal_dag, cx, cy, xy = dag_generator(clab=clab, xlab=xlab, ylab=ylab)
            cy.set_opacity(.3)
            indep = Tex(ylab + " " + r"$\perp \!\!\!\! \perp$" +  " " + xlab + " " + r"$\; | \;$" + " " + clab).next_to(causal_dag, DOWN)
            return VGroup(causal_dag, indep)
        
        current_dag_and_eq = VGroup(causal_dag, indep)
        fairness = dag_and_eq("Protected", "Ability", "Performance")
        self.play(TransformMatchingShapes(current_dag_and_eq, fairness))
        self.wait(.2)
        self.next_slide()

        sports = dag_and_eq("Game", "Participants", "Results")
        self.play(TransformMatchingShapes(fairness, sports))
        self.wait(.2)
        self.next_slide()

        lacctic = Text("https://www.lacctic.com/", color=BLUE).scale(.5).to_edge(DOWN, buff=1)
        self.play(Write(lacctic))
        self.wait(.2)
        self.next_slide()
        leftover = VGroup(sports, lacctic)
        return title, leftover
    
    def thanks(self, title, leftover):
        title = self.change_title("Thanks", title, leftover)

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

        dartmouth = Text("I am recruiting students for my lab at Dartmouth Engineering!").scale(.5).next_to(things, DOWN, buff = .5)
        self.play(Create(dartmouth))
        self.wait(.2)
        return title, leftover

    def construct(self):
        # Introduction
        leftover = None
        title = None
        self.intro()
        title, leftover = self.mushrooms_and_turkeys()
        title, leftover = self.introduce_mtes(title, leftover)

        # Problem 1
        title, leftover = self.confounding(title, leftover) # Potential outcomes and ATEs
        title, leftover, remember = self.causal_dags(title, leftover) # Generative model, explain exchangeabiolity/counterfactual DAG, then show unobserved
        title, leftover = self.spos_ate(title, leftover, remember)

        # Problem 2
        title, leftover = self.mixtures_of_relationships(title, leftover)
        title, leftover = self.tensor_methods(title, leftover)
        title, leftover = self.coins(title, leftover)
        title, leftover = self.spos_mte(title, leftover)

        # Conclusion
        title, leftover = self.takeaways(title, leftover)
        title, leftover = self.outlook(title, leftover)
        title, leftover = self.thanks(title, leftover)







