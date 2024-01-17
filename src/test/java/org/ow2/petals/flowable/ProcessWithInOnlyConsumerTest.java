/**
 * Copyright (c) 2017-2024 Linagora
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.ow2.petals.component.framework.test.Assert.assertMonitConsumerExtBeginLog;
import static org.ow2.petals.component.framework.test.Assert.assertMonitConsumerExtEndLog;
import static org.ow2.petals.component.framework.test.Assert.assertMonitConsumerExtFailureLog;
import static org.ow2.petals.component.framework.test.Assert.assertMonitProviderBeginLog;
import static org.ow2.petals.component.framework.test.Assert.assertMonitProviderEndLog;
import static org.ow2.petals.component.framework.test.Assert.assertMonitProviderFailureLog;

import java.util.List;
import java.util.logging.LogRecord;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.transform.Source;

import org.junit.jupiter.api.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.helpers.MessageChecks;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToConsumerMessage;
import org.ow2.petals.se_flowable.unit_test.in_only.Start;
import org.ow2.petals.se_flowable.unit_test.in_only.StartResponse;
import org.ow2.petals.se_flowable.unit_test.in_only.archivageservice.Archiver;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services with MEP 'InOnly'
 * 
 * @author Christophe DENEUX - Linagora
 */
public class ProcessWithInOnlyConsumerTest extends ProcessWithInOnlyConsumerTestEnvironment {

    /**
     * <p>
     * Check the service invocation with MEP InOnly returning an acknowledge
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the process instance is automatically completed when all is correctly done,</li>
     * <li>MONIT traces are as expected.</li>
     * </ul>
     */
    @Test
    public void nominal() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, INONLY_SU,
                OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT.sendAndCheckResponseAndSendStatus(request, new ServiceProviderImplementation() {
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
                assertEquals(ExchangeStatus.ACTIVE, this.msgExchange.getStatus());
                assertEquals(MEPPatternConstants.IN_ONLY, MEPPatternConstants.fromURI(this.msgExchange.getPattern()));
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertInstanceOf(Archiver.class, requestObj);

                // Returns the reply of the service provider to the Flowable service task
                return new StatusToConsumerMessage(requestMsg, ExchangeStatus.DONE);
            }

            @Override
            public boolean statusExpected() {
                return false;
            }
        }, new MessageChecks() {

            @Override
            public void checks(final Message message) throws Exception {
                // Check the reply
                final Source fault = message.getFault();
                assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
                assertNotNull(message.getPayload(), "No XML payload in response");
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertInstanceOf(StartResponse.class, responseObj);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString());

        // Check MONIT traces
        final List<LogRecord> monitLogs = COMPONENT_UNDER_TEST.getInMemoryLogHandler().getAllRecords(Level.MONIT);
        assertEquals(6, monitLogs.size());
        final FlowLogData initialInteractionRequestFlowLogData = assertMonitProviderBeginLog(INONLY_INTERFACE,
                INONLY_SERVICE, INONLY_ENDPOINT, OPERATION_START, monitLogs.get(0));
        final FlowLogData processBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs.get(1));

        assertMonitProviderEndLog(initialInteractionRequestFlowLogData, this.extractProviderEnd(
                initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME), monitLogs));

        final FlowLogData serviceTaskRequestBeginFlowLogData = assertMonitProviderBeginLog(processBeginFlowLogData,
                ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION,
                this.extractProviderBegin(ARCHIVE_ENDPOINT, monitLogs));
        assertMonitProviderEndLog(serviceTaskRequestBeginFlowLogData, this.extractProviderEnd(
                serviceTaskRequestBeginFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME), monitLogs));

        assertMonitConsumerExtEndLog(processBeginFlowLogData, this
                .extractConsumerExtEnd(processBeginFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME), monitLogs));

    }

    /**
     * <p>
     * Check the service invocation with MEP InOnly when an error occurs.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the service task is automatically set as dead letter job,</li>
     * <li>MONIT traces are as expected.</li>
     * </ul>
     */
    @Test
    public void error() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, INONLY_SU,
                OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        final ServiceProviderImplementation archiveServiceMock = new ServiceProviderImplementation() {
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
                assertEquals(ExchangeStatus.ACTIVE, this.msgExchange.getStatus());
                assertEquals(MEPPatternConstants.IN_ONLY, MEPPatternConstants.fromURI(this.msgExchange.getPattern()));
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertInstanceOf(Archiver.class, requestObj);

                // Returns the reply of the service provider to the Flowable service task
                return new StatusToConsumerMessage(requestMsg,
                        new Exception("An error occurs during processing of archiving service."));
            }

            @Override
            public boolean statusExpected() {
                return false;
            }
        };

        final ResponseMessage response = COMPONENT.sendAndGetResponse(request, archiveServiceMock);
        new MessageChecks() {

            @Override
            public void checks(final Message message) throws Exception {
                // Check the reply
                final Source fault = message.getFault();
                assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
                assertNotNull(message.getPayload(), "No XML payload in response");
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertInstanceOf(StartResponse.class, responseObj);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }.checks(response);

        // First retry by Flowable
        COMPONENT.receiveRequestAsExternalProvider(archiveServiceMock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT,
                false);

        // 2nd retry by Flowable
        COMPONENT.receiveRequestAsExternalProvider(archiveServiceMock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT,
                false);

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());

        // Cancel the process instance
        ((FlowableSE) COMPONENT_UNDER_TEST.getComponentObject()).cancelProcessInstance(processInstanceId.toString(),
                "Unrecoverable technical error !!");

        // Check MONIT traces
        final List<LogRecord> monitLogs = COMPONENT_UNDER_TEST.getInMemoryLogHandler().getAllRecords(Level.MONIT);
        assertEquals(10, monitLogs.size());

        final FlowLogData initialInteractionRequestFlowLogData = assertMonitProviderBeginLog(INONLY_INTERFACE,
                INONLY_SERVICE, INONLY_ENDPOINT, OPERATION_START, monitLogs.get(0));
        final FlowLogData processBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs.get(1));

        assertMonitProviderEndLog(initialInteractionRequestFlowLogData, this.extractProviderEnd(
                initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME), monitLogs));

        final FlowLogData serviceTaskRequestBeginFlowLogData_1 = assertMonitProviderBeginLog(processBeginFlowLogData,
                ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION,
                this.extractProviderBegin(ARCHIVE_ENDPOINT, monitLogs));
        assertMonitProviderFailureLog(serviceTaskRequestBeginFlowLogData_1, this.extractProviderFailure(
                serviceTaskRequestBeginFlowLogData_1.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME), monitLogs));
        final FlowLogData serviceTaskRequestBeginFlowLogData_2 = assertMonitProviderBeginLog(processBeginFlowLogData,
                ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION,
                this.extractProviderBegin(ARCHIVE_ENDPOINT, monitLogs));
        assertMonitProviderFailureLog(serviceTaskRequestBeginFlowLogData_2, this.extractProviderFailure(
                serviceTaskRequestBeginFlowLogData_2.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME), monitLogs));
        final FlowLogData serviceTaskRequestBeginFlowLogData_3 = assertMonitProviderBeginLog(processBeginFlowLogData,
                ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION,
                this.extractProviderBegin(ARCHIVE_ENDPOINT, monitLogs));
        assertMonitProviderFailureLog(serviceTaskRequestBeginFlowLogData_3, this.extractProviderFailure(
                serviceTaskRequestBeginFlowLogData_3.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME), monitLogs));

        assertMonitConsumerExtFailureLog(processBeginFlowLogData, this.extractConsumerExtFailure(
                processBeginFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME), monitLogs));
    }
}
