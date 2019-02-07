/**
 * Copyright (c) 2019 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.monit;

import static com.jayway.awaitility.Awaitility.await;
import static com.jayway.awaitility.Duration.TEN_SECONDS;

import java.net.URL;
import java.util.List;
import java.util.Optional;
import java.util.logging.LogRecord;

import javax.jbi.messaging.ExchangeStatus;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.TabularData;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.job.api.Job;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.component.framework.jbidescriptor.generated.MEPType;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.monitoring.business.filtering.AbstractMonitTraceFilteringTestForSimpleOrchestration;
import org.ow2.petals.component.framework.junit.monitoring.business.filtering.exception.ServiceProviderCfgCreationError;
import org.ow2.petals.flowable.FlowableSE;
import org.ow2.petals.flowable.FlowableSEConstants;
import org.ow2.petals.flowable.monitoring.MonitoringMBean;
import org.ow2.petals.unit_tests.se.flowable.monit.filtering.ObjectFactory;
import org.ow2.petals.unit_tests.se.flowable.monit.filtering.echo.EchoHelloResponse;
import org.ow2.petals.unit_tests.se.flowable.monit.filtering.echo.NoWall;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;
import com.ebmwebsourcing.easycommons.lang.reflect.ReflectionHelper;

/**
 * <p>
 * Unit tests about MONIT trace filtering for EIP pattern 'Router'.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class MonitTraceFilteringTest extends AbstractMonitTraceFilteringTestForSimpleOrchestration {

    private static final String PROVIDER_START_NS = "http://petals.ow2.org/unit-tests/se/flowable/monit/filtering";

    private static final QName PROVIDER_START_INTERFACE = new QName(PROVIDER_START_NS, "monit-filtering");

    private static final QName PROVIDER_START_SERVICE = new QName(PROVIDER_START_NS, "monit-filtering-service");

    private static final QName PROVIDER_START_OPERATION = new QName(PROVIDER_START_NS, "start");

    private static final QName PROVIDER_START_INONLY_OPERATION = new QName(PROVIDER_START_NS, "start-in-only");

    private static final QName PROVIDER_START_ROBUSTINONLY_OPERATION = new QName(PROVIDER_START_NS,
            "start-robust-in-only");

    private static final String PROVIDER_START_ENDPOINT = "edpMonitFiltering";

    private static final String CONSUMED_ECHO_NS = "http://petals.ow2.org/unit-tests/se/flowable/monit/filtering/echo";

    private static final QName CONSUMED_ECHO_INTERFACE = new QName(CONSUMED_ECHO_NS, "EchoInterface");

    private static final QName CONSUMED_ECHO_SERVICE = new QName(CONSUMED_ECHO_NS, "EchoService");

    private static final QName CONSUMED_SAYHELLO_OPERATION = new QName(CONSUMED_ECHO_NS, "sayHello");

    private static final QName CONSUMED_ROBUSTSAYHELLO_OPERATION = new QName(CONSUMED_ECHO_NS, "robustSayHello");

    private static final QName CONSUMED_ECHOHELLO_OPERATION = new QName(CONSUMED_ECHO_NS, "echoHello");

    private static final String CONSUMED_ECHO_ENDPOINT = "consumedEchoEndpoint";

    private static final Marshaller MARSHALLER;

    static {
        try {
            final JAXBContext context = JAXBContext.newInstance(ObjectFactory.class,
                    org.ow2.petals.unit_tests.se.flowable.monit.filtering.echo.ObjectFactory.class);
            MARSHALLER = context.createMarshaller();
            MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    public MonitTraceFilteringTest() {
        super();

        this.componentUnderTest
                // Enable Job executor
                .setParameter(
                        new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                        Boolean.TRUE.toString())
                .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP,
                        FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME), "1000")
                .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP,
                        FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME), "1000")
                // REST API is not needed
                .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_ENABLE),
                        Boolean.FALSE.toString());
    }

    @Override
    protected String getConsumedServiceEndpoint() {
        return CONSUMED_ECHO_ENDPOINT;
    }

    @Override
    protected QName getConsumedServiceName() {
        return CONSUMED_ECHO_SERVICE;
    }

    @Override
    protected QName getConsumedServiceInterface() {
        return CONSUMED_ECHO_INTERFACE;
    }

    @Override
    protected QName getConsumedServiceOperation(final MEPPatternConstants mep) {
        return mep == MEPPatternConstants.IN_OUT ? CONSUMED_ECHOHELLO_OPERATION
                : mep == MEPPatternConstants.IN_ONLY ? CONSUMED_SAYHELLO_OPERATION : CONSUMED_ROBUSTSAYHELLO_OPERATION;
    }

    @Override
    protected Marshaller getMarshaller() {
        return MARSHALLER;
    }

    @Override
    protected QName getInvokedServiceProviderOperation(final MEPPatternConstants mep) {
        switch (mep) {
            case IN_OUT:
                return PROVIDER_START_OPERATION;
            case IN_ONLY:
                return PROVIDER_START_INONLY_OPERATION;
            default:
                // RobustInOnly
                return PROVIDER_START_ROBUSTINONLY_OPERATION;
        }
    }

    @Override
    protected MEPPatternConstants getMepForServiceProviderOperation(MEPPatternConstants mep) {
        return MEPPatternConstants.IN_OUT;
    }

    @Override
    protected Object createRequestPayloadToProvider(final MEPPatternConstants mep) {
        return new ObjectFactory().createStart();
    }

    @Override
    protected Object createResponsePayloadToProvider(final MEPPatternConstants mep, final boolean useAsFault) {
        if (useAsFault) {
            return new org.ow2.petals.unit_tests.se.flowable.monit.filtering.echo.ObjectFactory()
                    .createNoWall(new NoWall());
        } else {
            return new org.ow2.petals.unit_tests.se.flowable.monit.filtering.echo.ObjectFactory()
                    .createEchoHelloResponse(new EchoHelloResponse());
        }
    }

    @Override
    protected MEPPatternConstants[] getMepsSupportedByServiceProvider() {
        return new MEPPatternConstants[] { MEPPatternConstants.ROBUST_IN_ONLY, MEPPatternConstants.IN_ONLY,
                MEPPatternConstants.IN_OUT };
    }

    @Override
    protected ProvidesServiceConfiguration createServiceProvider(final int ruleIdx)
            throws ServiceProviderCfgCreationError {

        final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                .getResource("org/ow2/petals/flowable/monit/monitFiltering.wsdl");
        assertNotNull("Rule #" + ruleIdx + ": WSDL not found", wsdlUrl);
        final ProvidesServiceConfiguration serviceProviderCfg = new ProvidesServiceConfiguration(
                PROVIDER_START_INTERFACE, PROVIDER_START_SERVICE, PROVIDER_START_ENDPOINT, wsdlUrl);

        final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("org/ow2/petals/flowable/monit/startResponse.xsl");
        assertNotNull("Output XSL 'startResponse.xsl' not found", startResponseXslUrl);
        serviceProviderCfg.addResource(startResponseXslUrl);

        final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                .getResource("org/ow2/petals/flowable/monit/monitFiltering.bpmn");
        assertNotNull("BPMN file not found", bpmnUrl);
        serviceProviderCfg.addResource(bpmnUrl);

        final URL echoServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                .getResource("org/ow2/petals/flowable/monit/echo.wsdl");
        assertNotNull("echoService WSDL not found", echoServiceWsdlUrl);
        serviceProviderCfg.addResource(echoServiceWsdlUrl);

        serviceProviderCfg.setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"),
                "monitFiltering.bpmn");
        serviceProviderCfg.setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

        return serviceProviderCfg;
    }

    @Override
    protected ConsumesServiceConfiguration[] createServiceConsumers(final int ruleIdx, final MEPType mep) {

        final ConsumesServiceConfiguration consumerServiceCfg = new ConsumesServiceConfiguration(
                CONSUMED_ECHO_INTERFACE, CONSUMED_ECHO_SERVICE, CONSUMED_ECHO_ENDPOINT);
        switch (mep) {
            case IN_OUT:
                consumerServiceCfg.setOperation(CONSUMED_ECHOHELLO_OPERATION);
                break;
            case IN_ONLY:
                consumerServiceCfg.setOperation(CONSUMED_SAYHELLO_OPERATION);
                break;
            default:
                // RobustInOnly
                consumerServiceCfg.setOperation(CONSUMED_ROBUSTSAYHELLO_OPERATION);
                break;
        }
        consumerServiceCfg.setMEP(mep);

        return new ConsumesServiceConfiguration[] { consumerServiceCfg };
    }

    @Override
    protected void executeExchangeReturningResponse(final RequestToProviderMessage incomingRequest,
            final ServiceProviderImplementation mock, final MEPPatternConstants mep,
            final Optional<Boolean> expectedFlowTracingActivationState, final String ruleIdPrefix) throws Exception {

        // A first invocation to the service provider under test is needed to create a process instance. Always
        // succeeds.
        this.startProcessInstance(incomingRequest);

        // Wait the invocation of the service task, and mock it
        this.component.receiveRequestAsExternalProvider(mock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT, false);
        this.componentUnderTest.pollStatusFromConsumer();
    }

    @Override
    protected void assertMonitTracesForServiceInvocationWithResponse(final String ruleIdPrefix,
            final List<LogRecord> monitLogs, final boolean isProvMonitTraceExpected,
            final boolean isConsMonitTraceExpected, final ProvidesServiceConfiguration providerServiceCfg,
            final MEPPatternConstants mep) {

        this.assertMonitTraces(ruleIdPrefix, monitLogs, isProvMonitTraceExpected, isConsMonitTraceExpected,
                providerServiceCfg, mep, false, false);
    }

    @Override
    protected void executeExchangeReturningFault(final RequestToProviderMessage incomingRequest,
            final ServiceProviderImplementation mock, final MEPPatternConstants mep,
            final Optional<Boolean> expectedFlowTracingActivationState, final String ruleIdPrefix) throws Exception {

        // A first invocation to the service provider under test is needed to create a process instance. Always
        // succeeds.
        this.startProcessInstance(incomingRequest);

        // Wait the invocation of the service task, and mock it
        this.component.receiveRequestAsExternalProvider(mock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT, false);
        this.componentUnderTest.pollStatusFromConsumer();
    }

    @Override
    protected void assertMonitTracesForServiceInvocationWithFault(final String ruleIdPrefix,
            final List<LogRecord> monitLogs, final boolean isProvMonitTraceExpected,
            final boolean isConsMonitTraceExpected, final ProvidesServiceConfiguration providerServiceCfg,
            final MEPPatternConstants mep) {

        this.assertMonitTraces(ruleIdPrefix, monitLogs, isProvMonitTraceExpected, isConsMonitTraceExpected,
                providerServiceCfg, mep, true, true);
    }

    @Override
    protected void executeExchangeReturningStatus(final RequestToProviderMessage incomingRequest,
            final ServiceProviderImplementation mock, final MEPPatternConstants mep,
            final ExchangeStatus statusToReturn, final Optional<Boolean> expectedFlowTracingActivationState,
            final String ruleIdPrefix) throws Exception {

        // A first invocation to the service provider under test is needed to create a process instance. Always
        // succeeds.
        this.startProcessInstance(incomingRequest);

        // Wait the invocation of the service task, and mock it
        this.component.receiveRequestAsExternalProvider(mock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT, false);

        // Error returned with MEP InOut put the job as dead-letter job. We cancel the process instance to get the last
        // MONIT trace
        if (statusToReturn == ExchangeStatus.ERROR) {
            // Wait that the job is put as dead letter job before to cancel the process instance
            await().atMost(TEN_SECONDS).until(() -> {
                assertEquals(1,
                        this.getProcessEngine().getManagementService().createDeadLetterJobQuery().list().size());
            });
            final Job deadLetterJob = this.getProcessEngine().getManagementService().createDeadLetterJobQuery()
                    .singleResult();
            this.getProcessEngine().getRuntimeService().deleteProcessInstance(deadLetterJob.getProcessInstanceId(),
                    "Unrecoverable technical error !!");
        }
    }

    @Override
    protected void assertMonitTracesForServiceInvocationWithStatus(final String ruleIdPrefix,
            final List<LogRecord> monitLogs, final boolean isProvMonitTraceExpected,
            final boolean isConsMonitTraceExpected, final ProvidesServiceConfiguration providerServiceCfg,
            final ExchangeStatus statusToReturn, final MEPPatternConstants mep) {

        this.assertMonitTraces(ruleIdPrefix, monitLogs, isProvMonitTraceExpected, isConsMonitTraceExpected,
                providerServiceCfg, mep, statusToReturn != ExchangeStatus.DONE, false);
    }

    @Override
    protected void waitExchangeExecutionEnd(final Optional<ExchangeStatus> statusReturned, final boolean faultReturned,
            final String ruleIdPrefix, final MEPPatternConstants mep) throws Exception {

        assertTrue(this.componentUnderTest.getComponentObject().getMonitoringBean() instanceof MonitoringMBean);
        final MonitoringMBean monitoringMbean = (MonitoringMBean) this.componentUnderTest.getComponentObject()
                .getMonitoringBean();

        await().atMost(TEN_SECONDS).until(() -> {
            try {
                final TabularData processMetrics = monitoringMbean.getProcessDefinitions();
                final CompositeData processInstances = processMetrics
                        .get(new String[] { mep == MEPPatternConstants.IN_OUT ? "monit-filtering-in-out"
                                : mep == MEPPatternConstants.IN_ONLY ? "monit-filtering-in-only"
                                        : "monit-filtering-robust-in-only" });
                assertEquals(0, ((Long) processInstances.get("active")).longValue());
                assertEquals(0, ((Long) processInstances.get("suspended")).longValue());
            } catch (final OpenDataException e) {
                throw new AssertionError(e);
            }
        });
    }

    @Override
    protected void onExchangeExecutionStart(final String ruleIdPrefix) throws Exception {

        // Remove previous terminate processes
        final List<HistoricProcessInstance> historyProcInstances = this.getProcessEngine().getHistoryService()
                .createHistoricProcessInstanceQuery().list();
        for (final HistoricProcessInstance historyProcInstance : historyProcInstances) {
            this.getProcessEngine().getHistoryService().deleteHistoricProcessInstance(historyProcInstance.getId());
        }

    }

    private ProcessEngine getProcessEngine() {
        return (ProcessEngine) ReflectionHelper.getFieldValue(FlowableSE.class,
                ((FlowableSE) this.componentUnderTest.getComponentObject()), "flowableEngine", false);
    }

    /**
     * Do process instance creation, sending a first request to the service provider under test. This request always
     * succeeds.
     */
    private void startProcessInstance(final RequestToProviderMessage startRequest) throws Exception {
        final ResponseMessage response = this.component.sendAndGetResponse(startRequest);
        this.component.sendDoneStatus(response);
    }

    private void assertMonitTraces(String ruleIdPrefix, List<LogRecord> monitLogs, boolean isProvMonitTraceExpected,
            boolean isConsMonitTraceExpected, ProvidesServiceConfiguration providerServiceCfg,
            final MEPPatternConstants mep, final boolean isFailureExpected, final boolean faultReturned) {

        if (isProvMonitTraceExpected) {
            assertTrue(ruleIdPrefix, monitLogs.size() >= 4);

            final FlowLogData providerBeginFlowLogData = assertMonitProviderBeginLog(ruleIdPrefix,
                    providerServiceCfg.getInterfaceName(), providerServiceCfg.getServiceName(),
                    providerServiceCfg.getEndpointName(), this.getInvokedServiceProviderOperation(mep),
                    monitLogs.get(0));
            final FlowLogData processBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs.get(1));
            assertMonitProviderEndLog(ruleIdPrefix, providerBeginFlowLogData, monitLogs.get(2));
            if (isFailureExpected && !faultReturned) {
                assertMonitConsumerExtFailureLog(processBeginFlowLogData, monitLogs.get(5));
            } else {
                assertMonitConsumerExtEndLog(processBeginFlowLogData, monitLogs.get(5));
            }

            if (isConsMonitTraceExpected) {
                assertEquals(ruleIdPrefix, 6, monitLogs.size());
                final FlowLogData consumedBeginFlowLogData = assertMonitProviderBeginLog(ruleIdPrefix,
                        processBeginFlowLogData, this.getConsumedServiceInterface(), this.getConsumedServiceName(),
                        this.getConsumedServiceEndpoint(), this.getConsumedServiceOperation(mep), monitLogs.get(3));
                if (isFailureExpected) {
                    assertMonitProviderFailureLog(ruleIdPrefix, consumedBeginFlowLogData, monitLogs.get(4));
                } else {
                    assertMonitProviderEndLog(ruleIdPrefix, consumedBeginFlowLogData, monitLogs.get(4));
                }
            } else {
                assertEquals(ruleIdPrefix, 4, monitLogs.size());
                assertMonitConsumerExtEndLog(processBeginFlowLogData, monitLogs.get(3));
            }
        } else if (isConsMonitTraceExpected) {
            assert !isProvMonitTraceExpected;

            assertEquals(ruleIdPrefix, 2, monitLogs.size());
            final FlowLogData consumedBeginFlowLogData = assertMonitProviderBeginLog(ruleIdPrefix,
                    this.getConsumedServiceInterface(), this.getConsumedServiceName(),
                    this.getConsumedServiceEndpoint(), this.getConsumedServiceOperation(mep), monitLogs.get(0));
            if (isFailureExpected) {
                assertMonitProviderFailureLog(ruleIdPrefix, consumedBeginFlowLogData, monitLogs.get(1));
            } else {
                assertMonitProviderEndLog(ruleIdPrefix, consumedBeginFlowLogData, monitLogs.get(1));
            }
        } else {
            assertEquals(ruleIdPrefix, 0, monitLogs.size());
        }
    }
}
