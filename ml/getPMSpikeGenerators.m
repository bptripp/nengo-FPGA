function sg = getPMSpikeGenerators(n, varargin)
    % sg = getPMSpikeGenerators(n) creates a selection of spike generators 
    % with a wide range of parameters. 
    % 
    % n: number of neurons to include in each spike generator
    % sg: cell array of spike generators
    
    if nargin > 1
        rng(varargin{1});
    end

    %parameters of populations with gaussian-distributed intercepts 
    gp = [ 
        .005 .04 2/3 2 2; 
        .002 .02 2/3 1 .5; 
        .002 .02 1 2 1; 
        .003 .03 1.5 3 .2; 
        .004 .01 1 3 .2; 
        ]; 
    
    % parameters of populations with uniform distributions 
    up = [
        .002 .02 -1 1 200 400; %defalt parameters for years
        .003 .02 -.9 .9 50 100; 
        .005 .02 -1 1 30 80; 
        .002 .1  -1 1 50 100; 
        .002 .1  0 1 50 100;
        ];
    
    sg = cell(size(gp,1)+size(gp,2), 1);
    for i = 1:size(gp,1)
        sg{i} = getGaussianSG(n, gp(i,1), gp(i,2), gp(i,3), gp(i,4), gp(i,5));
    end
    
    for i = 1:size(up,1)
        sg{size(gp,1)+i} = getUniformSD(n, up(i,1), up(i,2), up(i,3), up(i,4), up(i,5), up(i,6));
    end
    
%     for i = 1:length(sg)
%         plotPopulation(sg{i}, 2), pause
%     end

end

function sg = getGaussianSG(n, tauRef, tauRC, interceptSD, scaleShape, scaleScale)
    intercepts = interceptSD * randn(1,n);
    maxRates = 50+50*rand(1,n); %dummy values
    V0 = 0;
    sg = LIFSpikeGenerator(.0005, tauRef, tauRC, intercepts, maxRates, V0);
    sg.scales = gamrnd(scaleShape, scaleScale*ones(1,n));
    sg.biases = 1 - sg.scales .* intercepts;
end

function sg = getUniformSD(n, tauRef, tauRC, interceptLow, interceptHigh, maxRateLow, maxRateHigh)
    intercepts = interceptLow + (interceptHigh-interceptLow)*rand(1,n);
    maxRates = maxRateLow + (maxRateHigh-maxRateLow)*rand(1,n);
    V0 = 0;
    sg = LIFSpikeGenerator(.0005, tauRef, tauRC, intercepts, maxRates, V0);
    sg.scales(sg.scales > 100) = 100;
    sg.biases = 1 - sg.scales .* intercepts;
end

function plotPopulation(sg, radius)
    pop = CosinePopulation(1, sg, 'test');
    points = -radius:.005:radius;
    rates = getRates(pop, points, 0, 0);
    plot(points, rates, 'k')
end
