/**
 * Copyright (c) 2015-2025 Linagora
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
package org.ow2.petals.flowable.integration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.ow2.petals.component.framework.test.Assert.assertMonitProviderBeginLog;
import static org.ow2.petals.component.framework.test.Assert.assertMonitProviderEndLog;
import static org.ow2.petals.component.framework.test.Assert.assertMonitProviderFailureLog;

import java.util.List;
import java.util.logging.LogRecord;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.flowable.VacationProcessTestEnvironment;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Demande;

public abstract class AbstractIntegrationServiceInvokations extends VacationProcessTestEnvironment {

    /**
     * <p>
     * Check the processing of an integration service operation when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is not compliant to the XML schema defined in WSDL</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    protected void testInvalidRequest_WsdlUncompliant(final String suName, final QName interfaceName,
            final QName serviceName, final QName operationName) throws Exception {

        final Demande request = new Demande();

        COMPONENT_UNDER_TEST.getInMemoryLogHandler().clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(COMPONENT_UNDER_TEST, suName,
                operationName, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);
        assertNull(responseMsg.getError(), "An error is set in the response");
        assertNull(responseMsg.getOut(), "An out is set in response");
        assertNotNull(responseMsg.getFault(), "No fault in response");

        final Object faultObj = UNMARSHALLER.unmarshal(responseMsg.getFault());
        assertInstanceOf(InvalidRequest.class, faultObj);
        final InvalidRequest invalidRequest = (InvalidRequest) faultObj;
        assertTrue(invalidRequest.getMessage().endsWith("is unexpected"));
        assertFalse(invalidRequest.getStacktrace().isEmpty());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        assertMonitLogsWithFailure(interfaceName, serviceName, operationName);
    }

    /**
     * <p>
     * Check the processing of the integration service operation when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is compliant to the XML schema defined in WSDL, for example a response used as
     * request</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    protected void testInvalidRequest_WsdlCompliant(final String suName, final QName interfaceName,
            final QName serviceName, final QName operationName, final Object request) throws Exception {

        COMPONENT_UNDER_TEST.getInMemoryLogHandler().clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(COMPONENT_UNDER_TEST, suName,
                operationName, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);
        assertNull(responseMsg.getError(), "An error is set in the response");
        assertNull(responseMsg.getOut(), "An out is set in response");
        assertNotNull(responseMsg.getFault(), "No fault in response");

        final Object faultObj = UNMARSHALLER.unmarshal(responseMsg.getFault());
        assertInstanceOf(InvalidRequest.class, faultObj);
        final InvalidRequest invalidRequest = (InvalidRequest) faultObj;
        assertTrue(invalidRequest.getMessage().endsWith("is unexpected"));
        assertFalse(invalidRequest.getStacktrace().isEmpty());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        assertMonitLogsWithFailure(interfaceName, serviceName, operationName);
    }

    /**
     * <p>
     * Check the processing of the integration service operation when:
     * </p>
     * <ul>
     * <li>an empty request is sent</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    protected void testInvalidRequest_Empty(final String suName, final QName interfaceName, final QName serviceName,
            final QName operationName) throws Exception {

        COMPONENT_UNDER_TEST.getInMemoryLogHandler().clear();
        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, suName,
                operationName, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), (Source) null);

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(request);
        assertNull(responseMsg.getError(), "An error is set in the response");
        assertNull(responseMsg.getOut(), "An out is set in response");
        assertNotNull(responseMsg.getFault(), "No fault in response");

        final Object faultObj = UNMARSHALLER.unmarshal(responseMsg.getFault());
        assertInstanceOf(InvalidRequest.class, faultObj);
        final InvalidRequest invalidRequest = (InvalidRequest) faultObj;
        assertTrue(invalidRequest.getMessage().endsWith("is empty"));
        assertFalse(invalidRequest.getStacktrace().isEmpty());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        assertMonitLogsWithFailure(interfaceName, serviceName, operationName);
    }

    /**
     * <p>
     * Check the processing of the integration service operation with the given request. No error and no fault expected.
     */
    protected Object testRequest(final String suName, final QName interfaceName, final QName serviceName,
            final QName operationName, final Object request) throws Exception {

        COMPONENT_UNDER_TEST.getInMemoryLogHandler().clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(COMPONENT_UNDER_TEST, suName,
                operationName, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);

        assertNull(responseMsg.getError(), "An error is set in the response");
        assertNull(responseMsg.getFault(), "A fault is set in the response");
        assertNotNull(responseMsg.getPayload(), "No XML payload in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        final List<LogRecord> monitLogs = COMPONENT_UNDER_TEST.getInMemoryLogHandler().getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(interfaceName, serviceName,
                COMPONENT_UNDER_TEST.getNativeEndpointName(serviceName), operationName, monitLogs.get(0));
        assertMonitProviderEndLog(providerBegin, monitLogs.get(1));

        return responseObj;
    }

    /**
     * <p>
     * Check the processing of the integration service operation with the given request. Fault expected.
     */
    protected Object testRequestWithFault(final String suName, final QName interfaceName, final QName serviceName,
            final QName operationName, final Object request) throws Exception {

        COMPONENT_UNDER_TEST.getInMemoryLogHandler().clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(COMPONENT_UNDER_TEST, suName,
                operationName, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);

        assertNull(responseMsg.getError(), "An error is set in the response");
        assertTrue(responseMsg.isFault(), "A XML payload is set in the response");
        assertNotNull(responseMsg.getFault(), "No fault in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getFault());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        final List<LogRecord> monitLogs = COMPONENT_UNDER_TEST.getInMemoryLogHandler().getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(interfaceName, serviceName,
                COMPONENT_UNDER_TEST.getNativeEndpointName(serviceName), operationName, monitLogs.get(0));
        assertMonitProviderFailureLog(providerBegin, monitLogs.get(1));

        return responseObj;
    }

    /**
     * Assertion about MONIT logs generated during an invocation with error of the integration operation
     */
    private void assertMonitLogsWithFailure(final QName interfaceName, final QName serviceName,
            final QName operationName) {
        final List<LogRecord> monitLogs = COMPONENT_UNDER_TEST.getInMemoryLogHandler().getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(interfaceName, serviceName,
                COMPONENT_UNDER_TEST.getNativeEndpointName(serviceName), operationName, monitLogs.get(0));
        assertMonitProviderFailureLog(providerBegin, monitLogs.get(1));
    }

}
