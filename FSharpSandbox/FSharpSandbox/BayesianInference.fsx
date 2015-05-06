// Example of Bayesian inference
// Copied from Luca Bolognese's WebLog > Bayesian inference in F#

// Let's start with a simple example: inferring the underlying attitude of a small baby by observing her actions. Let's call this particular small baby Maia. People always asks her father if she is a 'good' baby or not. Her father started to wonder how he can possibly know that. Being 'good' is not very clear, so he chooses to answer the related question if her attitude is generally happy, unhappy or simply quiet (a kind of middle ground).

/// Underlying unobservable, but assumed stationary, state of the process (baby). Theta.
type Attitude =
    | Happy
    | UnHappy
    | Quiet

// Her poor father doesn't have much to go with. He can just observe what she does. He decides, for the sake of simplifying things, to categorize her state at each particular moment as smiling, crying or looking silly (a kind of middle ground).

/// Observable data. y.    
type Action =
    | Smile
    | Cry
    | LookSilly
 
// The father now has to decide what does it mean for Maia to be of an happy attitude. Lacking an universal definition of happiness in terms of these actions, he makes one up. Maia would be considered happy if she smiles 60% of the times, she cries 20% of the times and looks silly the remaining 20% of the times. He might as well have experimented with "clearly happy/unhappy" babies to come up with those numbers.

/// Data to model the underlying process (baby)
let happyActions = [ Smile, 0.6; Cry, 0.2; LookSilly, 0.2]
let unHappyActions = [Smile, 0.2; Cry, 0.6; LookSilly, 0.2]
let quietActions = [Smile, 0.4; Cry, 0.3; LookSilly, 0.3]

// What does it mean exactly? Well, this father would call his wife at random times during the day and ask her if Maia is smiling, crying or looking silly. He would then keep track of the numbers and then "somehow" decide what her attitude is. The general idea is simple, the "somehow" part is not.

/// Generates a new uniformly distributed number between 0 and 1
let random =
    let rnd = new System.Random()
    rnd.NextDouble

// The implementation of pickOne gets technical. You don't need to understand it to understand the rest of this post. This is the beauty of encapsulation. You can start reading from after the next code snippet if you want to.

// 'pickOne' works by constructing the inverse cumulative distribution function for the probability distribution described by the Happy/UnHappy/Quiet/Actions lists. There is an entry on wikipedia that describes how this works and I don't wish to say more here except presenting the code.

/// Find the first value more or equal to a key in a seq<'a * 'b>.
/// The seq is assumed to be sorted
let findByKey key aSeq =
    aSeq |> Seq.find (fun (k, _) -> k >= key) |> snd

/// Simulate an inverse CDF given values and probabilities
let buildInvCdf valueProbs =
    let cdfValues =
        valueProbs
        |> Seq.scan (fun cd (_, p) -> cd + p) 0. 
        |> Seq.skip 1 
    let cdf =
        valueProbs
        |> Seq.map fst 
        |> Seq.zip cdfValues 
        |> Seq.cache     
    fun x -> cdf |> findByKey x

/// Picks an 'a in a seq<'a * float> using float as the probability to pick a particular 'a
let pickOne probs =
    let rnd = random ()
    let picker = buildInvCdf probs
    picker rnd

// We can now model Maia. We want our model to return a particular action depending on which attitude we assume Maia is in mostly. For example, if we assume she is an happy baby, we want our model to return Smile about 60% of the times. In essence, we want to model what happens when the (poor) father calls his (even poorer) wife. What would his wife tell him (assuming a particular attitude)? The general idea is expressed by the following:

/// Process (baby) modeling. How she acts if she is fundamentally happy, unhappy or quiet
let MaiaSampleDistribution attitude =
    match attitude with
    | Happy     -> pickOne happyActions
    | UnHappy   -> pickOne unHappyActions
    | Quiet     -> pickOne quietActions

// The 'pickOne' function simply picks an action depending on the probability of it being picked. The name sample distribution is statistic-lingo to mean 'what you observe' and indeed you just can observe Maia's actions, not her underlying attitude.


// Another way to describe Maia is more mathematically convenient and will be used in the rest of the post. This second model answers the question: what is the probability of observing an action assuming a particular attitude? The distribution of both actions and attitudes (observable variable and parameter) is called joint probability.

module List = // This extends the List module by assoc function
  let assoc key list = list |> List.find (fun item -> fst item = key) |> snd

/// Another, mathematically more convenient, way to model the process (baby)
let MaiaJointProb attitude action =
    match attitude with
    | Happy     -> happyActions |> List.assoc action
    | UnHappy   -> unHappyActions |> List.assoc action
    | Quiet     -> quietActions |> List.assoc action

// let res = MaiaJointProb Attitude.Happy Action.Smile // val res : float = 0.6

// "List.assoc" returns the value associated with a key in a list containing (key, value) pairs. Notice that in general, if you are observing a process, you don't know what its joint distribution is. But you can approximate it by running the MaiaSampleDistribution function on known babies many times and keeping track of the result. So, in theory, if you have a way to experiment with many babies with known attitudes, you can create such a joint distribution.

// We now have modeled our problem, this is the creative part. From now on, it is just execution. We'll get to that.

// The previous post ended on this note.

//let MaiaJointProb attitude action =
//    match attitude with
//    | Happy     -> happyActions |> List.assoc action
//    | UnHappy   -> unHappyActions |> List.assoc action
//    | Quiet     -> quietActions |> List.assoc action

// This is just a two by two matrix. It simply represents which probability is associated to an (attitude, action) tuple. It is useful to think about it in these terms, because it makes easier to grasp the following function:

/// Conditional probability of a mental state, given a particular observed action
let MaiaLikelihood action = fun attitude -> MaiaJointProb attitude action

// let res = MaiaLikelihood Action.Cry Attitude.UnHappy

// This is simply a row in the matrix. It answers the question: given that I observe a particular action, what is the probability that Maia has a certain attitude?. This is called “likelihood function” in statistics. Its general form is: given that a I observe an outcome, what is the probability that it is generated by a process with a particular parameter?

// A related question is then: what if I observe a sequence of independent actions? What is the probability that the baby has a certain attitude then? This is answered by the following:

/// Multiple applications of the previous conditional probabilities for a series of actions (multiplied)
let MaiaLikelihoods actions =
    let composeLikelihoods previousLikelihood action  = fun attitude -> previousLikelihood attitude * MaiaLikelihood action attitude        
    actions |> Seq.fold composeLikelihoods (fun attitude -> 1.0)

// It is a trivial extension of the previous function (really), once you know that to combine likelihoods you multiply them.

// We now need to describe what our prior is. A prior is our preconceived notion about a particular parameter (in this case the baby’s attitude). You might be tempted to express that notion with a single value, but that would be inaccurate. You need to indicate how confident you are about it. In statistics you do that by choosing a distribution for your belief. This is one of the beauties of Bayesian statistics, everything is a probability distribution. In this case, we really don’t have any previous belief, so we pick the uniform distribution.

// (Initial attitude od Maia
let MaiaUniformPrior attitude = 1. / 3.

// Think of this as: you haven’t read any baby-attitude-specific study or received any external information about the likely attitude of Maia, so you cannot prefer one attitude over another.

// We are almost done. Now we have to apply the Bayesian theorem and get the un-normalized posterior distribution. Forget about the un-normalized word. What is a posterior distribution? This is your output, your return value. It says: given my prior belief on the value of a parameter and given the outcomes that I observed, this is what I now believe the parameter to be. In this case it goes like: I had no opinion on Maia’s attitude to start with, but after I observed her behavior for a while, I now think she is Happy with probability X, UnHappy with probability Y and Quiet with probability Z.

/// Calculates the unNormalized posterior given prior and likelihood
let unNormalizedPosterior (prior:'a -> float) likelihood =
    fun theta -> prior theta * likelihood theta

// We then need to normalize this thing (it doesn’t sum to one). The way to do it is to divide each probability by the sum of the probabilities for all the possible outcomes.

/// All possible values for the unobservable parameter (mental state)
let support = [Happy; UnHappy; Quiet]

/// Normalize the posterior (it integrates to 1.)
let posterior prior likelihood =
    let post = unNormalizedPosterior prior likelihood
    let sum = support |> List.sumBy (fun attitude -> post attitude)
    fun attitude -> post attitude / sum

// We are done. Now we can now start modeling scenarios. Let’s say that you observe [Smile;Smile;Cry;Smile;LookSilly]. What could the underlying attitude of Maia be?

let maiaIsANormalBaby = posterior MaiaUniformPrior (MaiaLikelihoods [Smile;Smile;Cry;Smile;LookSilly])

// We can then execute our little model:

let res1 = maiaIsANormalBaby Happy
let res2 = maiaIsANormalBaby UnHappy
let res3 = maiaIsANormalBaby Quiet

// And we get (0.5625, 0.0625, 0.375). So Maia is likely to be happy and unlikely to be unhappy. Let’s now model one extreme case:

// Biased initial distribution to experimet with
let MaiaBiasedPrior attitude =
  match attitude with
  | UnHappy -> 0.999
  | _ -> 0.001

/// Extreme cases
let maiaIsLikelyHappyDist = posterior MaiaBiasedPrior (MaiaLikelihoods [Smile;Smile;Smile;Smile;Smile;Smile;Smile])

let res4 = maiaIsLikelyHappyDist Happy
let res5 = maiaIsLikelyHappyDist UnHappy
let res6 = maiaIsLikelyHappyDist Quiet

// And we get (0.944, 0.000431, 0.05). Now Maia is almost certainly Happy. Notice that I can confidently make this affirmation because my end result is exactly what I was looking for when I started my quest. Using classical statistics, that wouldn’t be the case.

// A related question I might want to ask is: given the posterior distribution for attitude that I just found, what is the probability of observing a particular action? In other words, given the model that I built, what does it predict?

let posteriorPredictive jointProb posterior =
    let composeProbs previousProbs attitude = fun action -> previousProbs action + jointProb attitude action * posterior attitude  
    support |> Seq.fold composeProbs (fun action -> 0.0)
    
let nextLikelyUnknownActionDist = posteriorPredictive MaiaJointProb maiaIsLikelyHappyDist

// I don’t have the strength right now to explain the mathematical underpinning of this. In words, this says: considering that Maia can have one of the possible three Attitudes with the probability calculated above, what is the probability that I observe a particular action? Notice that the signature for it is: (Action –> float), which is the compiler way to say it.

// Now we can run the thing.

let res7 = nextLikelyUnknownActionDist Smile
let res8 = nextLikelyUnknownActionDist Cry
let res9 = nextLikelyUnknownActionDist LookSilly

// And we get (0.588, 0.2056, 0.2055). Why is that? We’ll talk about it in the next post.

let fact x = [1..x] |> List.fold (*) 1
let com n p = fact n / fact p / fact (n-p)
let per n = fact n
let var n p = fact n / fact (n-p)
