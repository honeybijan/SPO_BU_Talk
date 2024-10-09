from manim_slides import Slide, ThreeDSlide
import random
from manim import *
from GraphManim import *
from scipy.stats import multivariate_normal
from icons import *

from GraphManim import *

class Methods(Slide):

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




