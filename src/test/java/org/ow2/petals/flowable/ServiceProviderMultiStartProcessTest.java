/**
 * Copyright (c) 2017-2020 Linagora
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
package org.ow2.petals.flowable;

import java.util.List;
import java.util.logging.LogRecord;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.commons.log.PetalsExecutionContext;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.MessageChecks;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.ResponseToConsumerMessage;
import org.ow2.petals.flowable.monitoring.FlowableActivityFlowStepData;
import org.ow2.petals.flowable.monitoring.ProcessInstanceFlowStepBeginLogData;
import org.ow2.petals.se_flowable.unit_test.multi_start.StartByOnlineAgent;
import org.ow2.petals.se_flowable.unit_test.multi_start.StartByWeb;
import org.ow2.petals.se_flowable.unit_test.multi_start.StartResponse;
import org.ow2.petals.se_flowable.unit_test.multi_start.archivageservice.Archiver;
import org.ow2.petals.se_flowable.unit_test.multi_start.archivageservice.ArchiverResponse;
import org.ow2.petals.se_flowable.unit_test.multi_start.coreservice.Execute;
import org.ow2.petals.se_flowable.unit_test.multi_start.coreservice.ExecuteResponse;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services, with a component configured with default values and a process
 * with multi start events
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ServiceProviderMultiStartProcessTest extends MultistartProcessTestEnvironment {

    private static final String CUSTOMER_ADRESS = "customer adress";

    private static final String AGENT = "agent identifier";

    private static final String NO_AGENT = "";

    /**
     * <p>
     * Check the message processing where: a valid request is sent to create a new process instance from web channel.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the process instance is automatically completed with the right service call.</li>
     * </ul>
     */
    @Test
    public void web() throws Exception {

        // -------------------------------------------------------------------------------
        // ---- Create a new instance of the process definition through the channel 'Web'
        // -------------------------------------------------------------------------------
        final StartByWeb startByWebBean = new StartByWeb();
        startByWebBean.setCustomer(BPMN_USER);
        startByWebBean.setAddress(CUSTOMER_ADRESS);

        // Send the 1st valid request for start event 'request
        final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST, MULTISTART_SU,
                OPERATION_START_BY_WEB, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                toByteArray(startByWebBean));

        // Assert the response of the 1st valid request
        final ServiceProviderImplementation coreProcessServiceImpl = this.getCoreServiceTaskImpl(new ExecuteResponse(),
                NO_AGENT);
        final StringBuilder caseFileNumber = new StringBuilder();
        COMPONENT.sendAndCheckResponseAndSendStatus(request_1, coreProcessServiceImpl, new MessageChecks() {

            @Override
            public void checks(final Message message) throws Exception {
                // Check the reply
                final Source fault = message.getFault();
                assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                assertNotNull("No XML payload in response", message.getPayload());
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertTrue(responseObj instanceof StartResponse);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                caseFileNumber.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        // Wait the end of the process instance
        this.waitEndOfProcessInstance(caseFileNumber.toString());

        // Assertions about state of process instance at Flowable Level
        this.assertProcessInstanceFinished(caseFileNumber.toString());

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(6, monitLogs_1.size());
        final FlowLogData initialInteractionRequestFlowLogData = assertMonitProviderBeginLog(MULTISTART_INTERFACE,
                MULTISTART_SERVICE, MULTISTART_ENDPOINT, OPERATION_START_BY_WEB, monitLogs_1.get(0));
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        assertEquals(initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME),
                processStartedBeginFlowLogData.get(FlowableActivityFlowStepData.CORRELATED_FLOW_INSTANCE_ID_KEY));
        assertEquals(initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME),
                processStartedBeginFlowLogData.get(FlowableActivityFlowStepData.CORRELATED_FLOW_STEP_ID_KEY));
        assertEquals("multi-start",
                processStartedBeginFlowLogData.get(ProcessInstanceFlowStepBeginLogData.PROCESS_DEFINITION_KEY));
        assertEquals(caseFileNumber.toString(),
                processStartedBeginFlowLogData.get(ProcessInstanceFlowStepBeginLogData.PROCESS_INSTANCE_ID_KEY));
        // No assertion on next MONIT traces because their ordering is variable
        // TODO: Find a way to add assertion about these unordered MONIT traces

    }

    /**
     * <p>
     * Check the message processing where: a valid request is sent to create a new process instance from channel using
     * an online agent.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the process instance is automatically completed with the right service calls.</li>
     * </ul>
     */
    @Test
    public void onlineAgent() throws Exception {

        // ----------------------------------------------------------------------------------------
        // ---- Create a new instance of the process definition through the channel 'Online agent'
        // ----------------------------------------------------------------------------------------
        final StartByOnlineAgent startByOnlineAgentBean = new StartByOnlineAgent();
        startByOnlineAgentBean.setCustomer(BPMN_USER);
        startByOnlineAgentBean.setAddress(CUSTOMER_ADRESS);
        startByOnlineAgentBean.setAgent(AGENT);

        // Send the 1st valid request for start event 'request
        final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST, MULTISTART_SU,
                OPERATION_START_BY_ONLINE_AGENT, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                toByteArray(startByOnlineAgentBean));

        // Assert the response of the 1st valid request
        final ServiceProviderImplementation archiveAttachmentsServiceImpl = this
                .getArchiveAttachmentsServiceImpl(new ArchiverResponse());
        final ServiceProviderImplementation coreProcessServiceImpl = this.getCoreServiceTaskImpl(new ExecuteResponse(),
                AGENT);
        PetalsExecutionContext.clear();
        COMPONENT_UNDER_TEST.pushRequestToProvider(request_1);
        COMPONENT.receiveResponseAsExternalProvider(archiveAttachmentsServiceImpl, true);
        COMPONENT.receiveStatusAsExternalProvider(archiveAttachmentsServiceImpl);
        COMPONENT.receiveResponseAsExternalProvider(coreProcessServiceImpl, true);
        COMPONENT.receiveStatusAsExternalProvider(coreProcessServiceImpl);
        final ResponseMessage responseMsg_1 = COMPONENT_UNDER_TEST.pollResponseFromProvider();

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof StartResponse);
        final StartResponse response_1 = (StartResponse) responseObj_1;
        assertNotNull(response_1.getCaseFileNumber());

        COMPONENT.sendDoneStatus(responseMsg_1);

        // Wait the end of the process instance
        this.waitEndOfProcessInstance(response_1.getCaseFileNumber());

        // Assertions about state of process instance at Flowable Level
        this.assertProcessInstanceFinished(response_1.getCaseFileNumber());

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(8, monitLogs_1.size());
        final FlowLogData initialInteractionRequestFlowLogData = assertMonitProviderBeginLog(MULTISTART_INTERFACE,
                MULTISTART_SERVICE, MULTISTART_ENDPOINT, OPERATION_START_BY_ONLINE_AGENT, monitLogs_1.get(0));
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        assertEquals(initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME),
                processStartedBeginFlowLogData.get(FlowableActivityFlowStepData.CORRELATED_FLOW_INSTANCE_ID_KEY));
        assertEquals(initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME),
                processStartedBeginFlowLogData.get(FlowableActivityFlowStepData.CORRELATED_FLOW_STEP_ID_KEY));
        assertEquals("multi-start",
                processStartedBeginFlowLogData.get(ProcessInstanceFlowStepBeginLogData.PROCESS_DEFINITION_KEY));
        assertEquals(response_1.getCaseFileNumber(),
                processStartedBeginFlowLogData.get(ProcessInstanceFlowStepBeginLogData.PROCESS_INSTANCE_ID_KEY));
        // No assertion on next MONIT traces because their ordering is variable
        // TODO: Find a way to add assertion about these unordered MONIT traces

    }

    private ServiceProviderImplementation getArchiveAttachmentsServiceImpl(final ArchiverResponse responseBean) {

        return new ServiceProviderImplementation() {
            private MessageExchange msgExchange;

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                this.msgExchange = requestMsg.getMessageExchange();
                assertNotNull(this.msgExchange);
                assertEquals(ARCHIVE_INTERFACE, this.msgExchange.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, this.msgExchange.getService());
                assertNotNull(this.msgExchange.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, this.msgExchange.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_OPERATION, this.msgExchange.getOperation());
                assertEquals(this.msgExchange.getStatus(), ExchangeStatus.ACTIVE);
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Archiver);

                // Returns the reply of the service provider to the Flowable service task
                return new ResponseToConsumerMessage(requestMsg, toByteArray(responseBean));
            }

            @Override
            public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg);
                // It's the same message exchange instance
                assertSame(statusDoneMsg.getMessageExchange(), this.msgExchange);
                assertEquals(statusDoneMsg.getMessageExchange().getStatus(), ExchangeStatus.DONE);
            }
        };
    }

    private ServiceProviderImplementation getCoreServiceTaskImpl(final ExecuteResponse responseBean,
            final String expectedAgent) {

        return new ServiceProviderImplementation() {
            private MessageExchange msgExchange;

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                this.msgExchange = requestMsg.getMessageExchange();
                assertNotNull(this.msgExchange);
                assertEquals(CORE_SVC_INTERFACE, this.msgExchange.getInterfaceName());
                assertEquals(CORE_SVC_SERVICE, this.msgExchange.getService());
                assertNotNull(this.msgExchange.getEndpoint());
                assertEquals(CORE_SVC_ENDPOINT, this.msgExchange.getEndpoint().getEndpointName());
                assertEquals(CORE_SVC_OPERATION, this.msgExchange.getOperation());
                assertEquals(this.msgExchange.getStatus(), ExchangeStatus.ACTIVE);
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Execute);
                final Execute requestBean = (Execute) requestObj;
                assertEquals(CUSTOMER_ADRESS, requestBean.getAddress());
                assertEquals(BPMN_USER, requestBean.getCustomer());
                // TODO: Add an assertion about 'requestBean.getOrderId()'
                assertEquals(expectedAgent, requestBean.getApprovedBy());

                // Returns the reply of the service provider to the Flowable service task
                return new ResponseToConsumerMessage(requestMsg, toByteArray(responseBean));
            }

            @Override
            public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg);
                // It's the same message exchange instance
                assertSame(statusDoneMsg.getMessageExchange(), this.msgExchange);
                assertEquals(statusDoneMsg.getMessageExchange().getStatus(), ExchangeStatus.DONE);
            }
        };
    }
}
