namespace Unsupervised

module PCA =

    open MathNet
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Statistics
    
    let covarianceMatrix (M:Matrix<float>) =
        let cols = M.ColumnCount
        let C = DenseMatrix.create cols cols Matrix.Zero
        for c1 in 0 .. (cols - 1) do
            C.[c1,c1] <- Statistics.Variance (M.Column c1)
            for c2 in (c1 + 1) .. (cols - 1) do
                let cov = Statistics.Covariance (M.Column c1, M.Column c2)
                C.[c1,c2] <- cov
                C.[c2,c1] <- cov
        C

    let normalize dim (observations:float[][]) = 

        let averages = 
            Array.init dim (fun i -> 
                observations
                |> Seq.averageBy (fun x -> x.[i]))

        let stdDevs = 
            Array.init dim (fun i -> 
                let avg = averages.[i]
                observations 
                |> Seq.averageBy (fun x -> 
                    pown (float x.[i] - avg) 2 |> sqrt))

        observations 
        |> Array.map (fun row ->
            row 
            |> Array.mapi (fun i x -> 
                (float x - averages.[i]) / stdDevs.[i]))

    let pca (observations:float[][]) =
        
        let factorization =
            observations
            |> Matrix.Build.DenseOfRowArrays
            |> covarianceMatrix
            |> Matrix.eigen
        
        let eigenValues = factorization.EigenValues
        let eigenVectors = factorization.EigenVectors

        let projector (obs:float[]) =
            let obsVector = obs |> Vector.Build.DenseOfArray
            (eigenVectors.Transpose () * obsVector)
            |> Vector.toArray

        (eigenValues,eigenVectors), projector