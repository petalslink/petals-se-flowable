/**
 * Copyright (c) 2017 Linagora
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

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.MessageChecks;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.message.FaultToConsumerMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToConsumerMessage;
import org.ow2.petals.se_flowable.unit_test.robust_in_only.Start;
import org.ow2.petals.se_flowable.unit_test.robust_in_only.StartResponse;
import org.ow2.petals.se_flowable.unit_test.robust_in_only.archivageservice.Archiver;
import org.ow2.petals.se_flowable.unit_test.robust_in_only.archivageservice.UnexistingId;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services with MEP 'RobustInOnly'
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ProcessWithRobustInOnlyConsumerTest extends ProcessWithRobustInOnlyConsumerTestEnvironment {

    /**
     * <p>
     * Check the service invocation with MEP RobustInOnly when no error or fault occurs
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the process instance is automatically completed when all is correctly done.</li>
     * </ul>
     * </p>
     */
    @Test
    public void nominal() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, ROBUSTINONLY_SU,
                OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT.sendAndCheckResponseAndSendStatus(request, new ServiceProviderImplementation() {

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                final MessageExchange msgExchange = requestMsg.getMessageExchange();
                assertNotNull(msgExchange);
                assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
                assertNotNull(msgExchange.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_OPERATION, msgExchange.getOperation());
                assertEquals(ExchangeStatus.ACTIVE, msgExchange.getStatus());
                assertEquals(MEPPatternConstants.ROBUST_IN_ONLY,
                        MEPPatternConstants.fromURI(msgExchange.getPattern()));
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Archiver);

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
                assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                assertNotNull("No XML payload in response", message.getPayload());
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertTrue(responseObj instanceof StartResponse);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString());

    }

    /**
     * <p>
     * Check the service invocation with MEP RobustInOnly when an error occurs
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the service task is automatically set as dead letter job.</li>
     * </ul>
     * </p>
     */
    @Test
    public void error() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, ROBUSTINONLY_SU,
                OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        final ServiceProviderImplementation archiveServiceMock = new ServiceProviderImplementation() {

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                final MessageExchange msgExchange = requestMsg.getMessageExchange();
                assertNotNull(msgExchange);
                assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
                assertNotNull(msgExchange.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_OPERATION, msgExchange.getOperation());
                assertEquals(MEPPatternConstants.ROBUST_IN_ONLY,
                        MEPPatternConstants.fromURI(msgExchange.getPattern()));
                assertEquals(ExchangeStatus.ACTIVE, msgExchange.getStatus());
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Archiver);

                // Returns the reply of the service provider to the Flowable service task
                return new StatusToConsumerMessage(requestMsg,
                        new Exception("An error occurs during processing of archiving service."));
            }

            @Override
            public boolean statusExpected() {
                return false;
            }
        };

        COMPONENT.sendAndCheckResponseAndSendStatus(request, archiveServiceMock, new MessageChecks() {

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
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        // First retry by Flowable
        COMPONENT.receiveRequestAsExternalProvider(archiveServiceMock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT,
                false);

        // 2nd retry by Flowable
        COMPONENT.receiveRequestAsExternalProvider(archiveServiceMock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT,
                false);

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());

    }

    /**
     * <p>
     * Check the service invocation with MEP RobustInOnly when an fault occurs, and the fault is not trapped at BPMN
     * level
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the service task is automatically set as dead letter job.</li>
     * </ul>
     * </p>
     */
    @Test
    public void faultNotTrapped() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, ROBUSTINONLY_SU,
                OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        final ServiceProviderImplementation archiveServiceMock = new ServiceProviderImplementation() {

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {


                final MessageExchange msgExchange = requestMsg.getMessageExchange();
                assertNotNull(msgExchange);
                assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
                assertNotNull(msgExchange.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_OPERATION, msgExchange.getOperation());
                assertEquals(MEPPatternConstants.ROBUST_IN_ONLY,
                        MEPPatternConstants.fromURI(msgExchange.getPattern()));
                assertEquals(ExchangeStatus.ACTIVE, msgExchange.getStatus());

                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Archiver);

                // Returns the reply of the service provider to the Flowable service task
                final UnexistingId fault = new UnexistingId();
                fault.setId("id");
                return new FaultToConsumerMessage(requestMsg, toByteArray(fault));
            }

            @Override
            public void handleStatus(final StatusMessage status) throws Exception {
                assertEquals(ExchangeStatus.DONE, status.getStatus());
            }

            @Override
            public boolean statusExpected() {
                return true;
            }
        };

        COMPONENT.sendAndCheckResponseAndSendStatus(request, archiveServiceMock, new MessageChecks() {

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
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        // First retry by Flowable
        COMPONENT.receiveRequestAsExternalProvider(archiveServiceMock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT,
                false);
        COMPONENT.receiveStatusAsExternalProvider(archiveServiceMock);

        // 2nd retry by Flowable
        COMPONENT.receiveRequestAsExternalProvider(archiveServiceMock, SimpleComponent.DEFAULT_SEND_AND_RECEIVE_TIMEOUT,
                false);
        COMPONENT.receiveStatusAsExternalProvider(archiveServiceMock);

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());

    }
}