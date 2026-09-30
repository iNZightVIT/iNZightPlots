export type PlotVariables = {
  v1: string;
  v2?: string;
  s1?: string;
  s2?: string;
  colby?: string;
  symbolby?: string;
  sizeby?: string;
};

export type PlotLayout = {
  matrix: boolean;
};

export type Box = {
  min: number;
  q1: number;
  median: number;
  q3: number;
  max: number;
};

export type BarData = {
  label: string[];
  count: number[];
  proportion: number[];
};

export type BarPanel = {
  s1?: string;
  s2?: string;
  total: number;
  data: BarData;
};

export type BarTwoWayPanel = {
  s1?: string;
  s2?: string;
  total: number;
  seriesTotals: { label: string[]; total: number[] };
  data: {
    label: string[];
    series: string[];
    count: number[];
    proportion: number[];
  };
};

export type BarSegmentPanel = {
  s1?: string;
  s2?: string;
  total: number;
  data: BarData;
  segments: {
    label: string[];
    segment: string[];
    count: number[];
    proportion: number[];
  };
};

export type DotPanel = {
  s1?: string;
  s2?: string;
  groups: Array<{
    label?: string;
    points: { x: number[] };
    boxplot?: Box;
    mean?: number;
  }>;
};

export type HistPanel = {
  s1?: string;
  s2?: string;
  edges: number[];
  groups: Array<{
    label?: string;
    counts: number[];
    boxplot?: Box;
    mean?: number;
  }>;
};

export type ScatterPanel = {
  s1?: string;
  s2?: string;
  data: {
    id: number[];
    x: number[];
    y: number[];
    size?: number[];
    symbol?: number[];
    colby?: Array<string | number | null>;
    highlight?: boolean[];
  };
};

export type HexPanel = {
  s1?: string;
  s2?: string;
  xBounds: [number, number];
  yBounds: [number, number];
  xBins: number;
  shape: number;
  data: {
    x: number[];
    y: number[];
    count: number[];
    meanX: number[];
    meanY: number[];
  };
};

export type GridPanel = {
  s1?: string;
  s2?: string;
  xBounds: [number, number];
  yBounds: [number, number];
  n: number;
  data: {
    x0: number[];
    x1: number[];
    y0: number[];
    y1: number[];
    count: number[];
  };
};

export type Panel =
  | BarPanel
  | BarTwoWayPanel
  | BarSegmentPanel
  | DotPanel
  | HistPanel
  | ScatterPanel
  | HexPanel
  | GridPanel;

export type Plot = {
  schemaVersion: 1;
  type: string;
  variables: PlotVariables;
  layout?: PlotLayout;
  panels: Panel[];
};
