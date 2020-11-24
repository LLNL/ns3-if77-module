/* -*- Mode:C++; c-file-style:"gnu"; indent-tabs-mode:nil; -*- */

// Include a header file from your module to test.
#include "ns3/if77-propagation-loss-model.h"

#include "ns3/core-module.h"
#include "ns3/mobility-module.h"

// An essential include is test.h
#include "ns3/test.h"

// Do not put your test classes in namespace ns3.  You may find it useful
// to use the using directive to access the ns3 namespace directly
using namespace ns3;

// This is an example TestCase.
class If77PropagationLossModelTestCase : public TestCase
{
public:
  /**
   * Default Constructor
   */
  If77PropagationLossModelTestCase ()
      : TestCase ("if77-propagation-loss-model")
  {}

  /**
   * Destructor
   */
  virtual ~If77PropagationLossModelTestCase ()
  {}

private:
  void TestModelConstruction ();
  void TestGetDefaultConfig ();
  void TestReceivedSignalAt1Kilometer();
  void TestReceivedSignalAt100Kilometers();
  virtual void DoRun (void);
};

void
If77PropagationLossModelTestCase::TestModelConstruction ()
{
    Ptr<If77PropagationLossModel> model = CreateObject<If77PropagationLossModel> ();

    NS_TEST_ASSERT_MSG_NE (model, 0,
                            "Failed to create If77 propagation model"); 
}

void
If77PropagationLossModelTestCase::TestGetDefaultConfig ()
{
    Ptr<If77PropagationLossModel> model = CreateObject<If77PropagationLossModel> ();

    auto& config = model->GetDefaultConfig ();

    bool useKmM = config.IK == if77::If77Config::IK_t::KM_M;
    NS_TEST_ASSERT_MSG_EQ (useKmM, true,
                          "Default configuration is not using Kilometers/meters");
}

void
If77PropagationLossModelTestCase::TestReceivedSignalAt1Kilometer()
{
    //Problem Parameters
    //Tx Power: 10dBW
    //Tx Antenna Height: 5m
    //RX Antenna Height: 5km
    //Frequency: 125Mhz
    //Horizontal Distance between antenna: 1km
    const double txPowerDbm = 10;  //TX power in dBm
    const double txHeight = 15;     //Tx antenna height in meters
    const double rxHeight = 5000;  //Rx antenna height in meters
    const double rxDistance = 1000;   //Distance between Tx and Rx in meters
    const double frequency = 125;   //Frequency in Mhz
    const double rxPowerDbm = -83.0; //expected dBm at receiver

    Ptr<If77PropagationLossModel> model = CreateObject<If77PropagationLossModel> ();
    model->SetAttribute ("Frequency", DoubleValue (frequency));

    Ptr<MobilityModel> transmitter = CreateObject<ConstantPositionMobilityModel> ();
    Ptr<MobilityModel> receiver = CreateObject<ConstantPositionMobilityModel> ();

    transmitter->SetAttribute ("Position", Vector3DValue (Vector(0, 0, txHeight)));
    receiver->SetAttribute ("Position", Vector3DValue (Vector (rxDistance, 0, rxHeight)));

    auto result = model->CalcRxPower (txPowerDbm, transmitter, receiver);

    NS_TEST_ASSERT_MSG_EQ_TOL (result, rxPowerDbm, 0.1,
                            "Receive power is not correct");
}

void
If77PropagationLossModelTestCase::TestReceivedSignalAt100Kilometers()
{
    //Problem Parameters
    //Tx Power: 40dBW
    //Tx Antenna Height: 15m
    //RX Antenna Height: 30km
    //Frequency: 125Mhz
    //Horizontal Distance between antenna: 100km
    const double txPowerDbm = 40;  //TX power in dBm
    const double txHeight = 15;     //Tx antenna height in meters
    const double rxHeight = 30000;  //Rx antenna height in meters
    const double rxDistance = 100000;   //Distance between Tx and Rx in meters
    const double frequency = 125;   //Frequency in Mhz
    const double rxPowerDbm = -72.3; //expected dBm at receiver

    Ptr<If77PropagationLossModel> model = CreateObject<If77PropagationLossModel> ();
    model->SetAttribute ("Frequency", DoubleValue (frequency));

    Ptr<MobilityModel> transmitter = CreateObject<ConstantPositionMobilityModel> ();
    Ptr<MobilityModel> receiver = CreateObject<ConstantPositionMobilityModel> ();

    transmitter->SetAttribute ("Position", Vector3DValue (Vector(0, 0, txHeight)));
    receiver->SetAttribute ("Position", Vector3DValue (Vector (rxDistance, 0, rxHeight)));

    auto result = model->CalcRxPower (txPowerDbm, transmitter, receiver);

    NS_TEST_ASSERT_MSG_EQ_TOL (result, rxPowerDbm, 0.1,
                            "Receive power is not correct");
}

//
// This method is the pure virtual method from class TestCase that every
// TestCase must implement
//
void
If77PropagationLossModelTestCase::DoRun (void)
{
    TestModelConstruction ();
    TestGetDefaultConfig ();
    TestReceivedSignalAt1Kilometer();
    TestReceivedSignalAt100Kilometers();
}

// The TestSuite class names the TestSuite, identifies what type of TestSuite,
// and enables the TestCases to be run.  Typically, only the constructor for
// this class must be defined
//
class If77PropagationLossModelTestSuite : public TestSuite
{
public:
  If77PropagationLossModelTestSuite ()
      : TestSuite ("if77", UNIT)
  {
      RegisterTestCases ();
  }

private:
  void RegisterTestCases ();
};

void
If77PropagationLossModelTestSuite::RegisterTestCases ()
{
  // TestDuration for TestCase can be QUICK, EXTENSIVE or TAKES_FOREVER
  AddTestCase (new If77PropagationLossModelTestCase (), TestCase::QUICK);
}

// Do not forget to allocate an instance of this TestSuite
static If77PropagationLossModelTestSuite g_if77PropagationLossModelTestSuite;

